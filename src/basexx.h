#pragma once

#include <algorithm>
#include <expected>
#include <functional>
#include <numeric>
#include <ranges>
#include <string_view>

enum class decodexx_error : signed char {
	illegal_character,
	missing_character,
	illegal_padding,
	non_canonical,
};

template <typename I, typename O>
struct decodexx_error_result {
	std::ranges::in_out_result<I, O> in_out_result;
	decodexx_error error;
};

namespace detail {
namespace basexx {

constexpr auto padding = '=';

enum class identifier : unsigned char {
	base16,
	base32,
	base32hex,
	base64,
};

template <identifier id>
struct alphabet {};

template <identifier id>
inline constexpr auto alphabet_v = alphabet<id>::value;

template <>
struct alphabet<identifier::base16> {
	static constexpr auto value = std::string_view{"0123456789ABCDEF"};
};

template <>
struct alphabet<identifier::base32> {
	static constexpr auto value = std::string_view{"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"};
};

template <>
struct alphabet<identifier::base32hex> {
	static constexpr auto value = std::string_view{"0123456789ABCDEFGHIJKLMNOPQRSTUV"};
};

template <>
struct alphabet<identifier::base64> {
	static constexpr auto value =
	    std::string_view{"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"};
};

template <identifier id>
inline constexpr std::size_t alphabet_size{alphabet_v<id>.size()};

template <identifier id>
struct shift_register {};

template <identifier id>
using shift_register_t = shift_register<id>::type;

template <>
struct shift_register<identifier::base16> {
	using type = unsigned char;
};

template <identifier id>
	requires(id == identifier::base32 or id == identifier::base32hex)
struct shift_register<id> {
	using type = unsigned long long;
};

template <>
struct shift_register<identifier::base64> {
	using type = unsigned long;
};

template <identifier id>
struct alphabet_properties {
	static constexpr int width{std::countr_zero(alphabet_size<id>)};
	static constexpr int octet_width{8};
	static constexpr int block_width{std::lcm(width, octet_width)};
	static constexpr int block_num_octets{block_width / octet_width};
	static constexpr int block_num_chars{block_width / width};

	static_assert(static_cast<int>(sizeof(shift_register_t<id>)) >= block_num_octets);
};

template <identifier id>
struct encode : std::ranges::range_adaptor_closure<encode<id>> {
	template <std::ranges::viewable_range R>
		requires(std::ranges::forward_range<R> and
			 std::convertible_to<std::ranges::range_reference_t<R>, std::byte>)
	constexpr auto operator()(R &&r) const
	{
		using props = basexx::alphabet_properties<id>;

		return std::views::cartesian_product(std::forward<R>(r) | std::views::chunk(props::block_num_octets),
						     std::views::iota(0, static_cast<int>(props::block_num_chars)) |
							 std::views::reverse) |
		       std::views::transform([](auto indexed_chunk) -> char {
			       auto [chunk, i] = indexed_chunk;
			       auto j = props::block_num_octets - 1;
			       unsigned char c{};
			       for (const auto octet : chunk) {
				       c |= std::rotr(static_cast<basexx::shift_register_t<id>>(octet),
						      (props::width * i) - (props::octet_width * j--));
			       }
			       return i < (props::block_num_chars -
					   (props::octet_width * (props::block_num_octets - j - 1) + props::width - 1) /
					       props::width)
					  ? basexx::padding
					  : alphabet_v<id>.at(c & (alphabet_size<id> - 1));
		       });
	}
};

} // namespace basexx

namespace decodexx {

template <basexx::identifier id>
class max_size {
	static constexpr auto input_size_to_max_size(size_t input_size) -> std::expected<size_t, decodexx_error>
	{
		using props = basexx::alphabet_properties<id>;

		if ((input_size % props::block_num_chars) != 0) {
			return std::unexpected{decodexx_error::missing_character};
		}

		return input_size / props::block_num_chars * props::block_num_octets;
	}

public:
	template <typename R>
		requires(std::ranges::sized_range<R> or std::ranges::forward_range<R>)
	// NOLINTNEXTLINE(cppcoreguidelines-missing-std-forward)
	constexpr auto operator()(R &&r) const -> std::expected<size_t, decodexx_error>
	{
		if constexpr (std::ranges::sized_range<R>) {
			return input_size_to_max_size(std::ranges::size(r));
		} else {
			return input_size_to_max_size(std::ranges::distance(r));
		}
	}
};

} // namespace decodexx

namespace basexx {

template <identifier id>
struct try_decode {
	template <std::input_iterator I, std::sentinel_for<I> S, std::weakly_incrementable O,
		  typename Proj = std::identity>
		requires(std::convertible_to<std::indirect_result_t<Proj, I>, char> and
			 std::indirectly_writable<O, std::byte>)
	constexpr auto operator()(I first, S last, O result, Proj proj = {}) const
	    -> std::expected<std::ranges::in_out_result<I, O>, decodexx_error_result<I, O>>
	{
		using namespace std::views;

		using lookup_t = std::array<unsigned char, std::numeric_limits<unsigned char>::max() + 1>;

		// The following flags would not work for any alphabet size, but they work up to 64, which is enough for
		// us.
		static constexpr auto is_valid = 0x40;
		static constexpr auto is_padding = 0x80;

		static constexpr auto lookup = [] -> lookup_t {
			lookup_t a{};
			unsigned char i{};
			for (const unsigned char c : basexx::alphabet_v<id>) {
				a.at(c) = is_valid | i++;
			}
			a.at(static_cast<unsigned char>('=')) = is_padding | is_valid;
			return a;
		}();

		auto make_error_result = [first = std::move(first),
					  result = std::move(result)](decodexx_error error) mutable {
			return std::unexpected{decodexx_error_result<I, O>{
			    .in_out_result = {std::move(first), std::move(result)},
			    .error = error,
			}};
		};

		while (first != last) {
			using props = alphabet_properties<id>;
			basexx::shift_register_t<id> shift_register{};
			auto num_padding = 0;
			auto i = 0;
			for (i = 0; i < props::block_num_chars and first != last; ++i, ++first) {
				const auto bitset = lookup.at(static_cast<unsigned char>(std::invoke(proj, *first)));
				if (not(bitset & is_valid)) {
					return make_error_result(decodexx_error::illegal_character);
				}
				const auto bitset_is_padding = static_cast<int>(bitset >> 7);
				if ((i < 2 and bitset_is_padding) or (num_padding and not bitset_is_padding)) {
					return make_error_result(decodexx_error::illegal_padding);
				}
				num_padding += bitset_is_padding;
				shift_register = (shift_register << props::width) | (bitset & (alphabet_size<id> - 1));
			}
			if (i != props::block_num_chars) {
				return make_error_result(decodexx_error::missing_character);
			}
			const int num_missing_octets{(num_padding * props::width + props::octet_width - 1) /
						     props::octet_width};
			if (shift_register &
			    ((decltype(shift_register){1} << (num_missing_octets * props::octet_width)) - 1)) {
				return make_error_result(decodexx_error::non_canonical);
			}
			for (i = 0; i < props::block_num_octets - num_missing_octets; ++i) {
				*result++ = static_cast<std::byte>(
				    shift_register >> ((props::block_num_octets - i - 1) * props::octet_width));
			}
		}

		return std::ranges::in_out_result<I, O>{std::move(first), std::move(result)};
	}

	template <std::ranges::input_range R, std::weakly_incrementable O, typename Proj = std::identity>
		requires(std::convertible_to<std::indirect_result_t<Proj, std::ranges::iterator_t<R>>, char> and
			 std::indirectly_writable<O, std::byte>)
	// NOLINTNEXTLINE(cppcoreguidelines-missing-std-forward)
	constexpr auto operator()(R &&r, O result, Proj proj = {}) const
	    -> std::expected<std::ranges::in_out_result<std::ranges::borrowed_iterator_t<R>, O>,
			     decodexx_error_result<std::ranges::borrowed_iterator_t<R>, O>>
	{
		return (*this)(std::ranges::begin(r), std::ranges::end(r), std::move(result), std::move(proj));
	}
};

template <identifier id, template <typename> typename C>
struct try_decode_to {
	template <std::ranges::input_range R, typename Proj = std::identity>
		requires std::convertible_to<std::indirect_result_t<Proj, std::ranges::iterator_t<R>>, char>
	constexpr auto operator()(R &&r, Proj proj = {}) const -> std::expected<C<std::byte>, decodexx_error>;
};

template <identifier id>
struct try_decode_to<id, std::vector> {
	template <std::ranges::input_range R, typename Proj = std::identity>
		requires std::convertible_to<std::indirect_result_t<Proj, std::ranges::iterator_t<R>>, char>
	constexpr auto operator()(R &&r, Proj proj = {}) const -> std::expected<std::vector<std::byte>, decodexx_error>
	{
		std::vector<std::byte> v{};

		return try_decode<id>{}(std::forward<R>(r), std::back_insert_iterator{v}, std::move(proj))
		    .transform([v = std::move(v)](auto &&) mutable { return std::move(v); })
		    .transform_error([](const auto &error) { return error.error; });
	}

	template <std::ranges::input_range R, typename Proj = std::identity>
		requires(std::convertible_to<std::indirect_result_t<Proj, std::ranges::iterator_t<R>>, char> and
			 (std::ranges::sized_range<R> or std::ranges::forward_range<R>))
	constexpr auto operator()(R &&r, Proj proj = {}) const -> std::expected<std::vector<std::byte>, decodexx_error>
	{
		const auto size = decodexx::max_size<id>{}(std::forward<R>(r));

		if (not size.has_value()) {
			return std::unexpected{size.error()};
		}

		std::vector<std::byte> v(*size);

		const auto result = try_decode<id>{}(std::forward<R>(r), v.begin(), std::move(proj));

		if (result.has_value()) {
			v.resize(result->out - v.begin());
		}

		return result.transform([v = std::move(v)](auto &&) mutable { return std::move(v); })
		    .transform_error([](const auto &error) { return error.error; });
	}
};

template <identifier id, template <typename> typename C>
	requires std::default_initializable<C<std::byte>>
struct decode_to {
	template <std::ranges::input_range R, typename Proj = std::identity>
		requires std::convertible_to<std::indirect_result_t<Proj, std::ranges::iterator_t<R>>, char>
	constexpr auto operator()(R &&r, Proj proj = {}) const -> C<std::byte>
	{
		return try_decode_to<id, C>{}(std::forward<R>(r), std::move(proj)).value_or(C<std::byte>{});
	}
};

} // namespace basexx

} // namespace detail

inline constexpr detail::basexx::encode<detail::basexx::identifier::base16> encode16{};
inline constexpr detail::basexx::encode<detail::basexx::identifier::base32> encode32{};
inline constexpr detail::basexx::encode<detail::basexx::identifier::base32hex> encode32hex{};
inline constexpr detail::basexx::encode<detail::basexx::identifier::base64> encode64{};

inline constexpr detail::decodexx::max_size<detail::basexx::identifier::base16> decode16_max_size{};
inline constexpr detail::decodexx::max_size<detail::basexx::identifier::base32> decode32_max_size{};
inline constexpr detail::decodexx::max_size<detail::basexx::identifier::base32hex> decode64_max_size{};

inline constexpr detail::basexx::try_decode<detail::basexx::identifier::base16> try_decode16{};
inline constexpr detail::basexx::try_decode<detail::basexx::identifier::base32> try_decode32{};
inline constexpr detail::basexx::try_decode<detail::basexx::identifier::base32hex> try_decode32hex{};
inline constexpr detail::basexx::try_decode<detail::basexx::identifier::base64> try_decode64{};

inline constexpr detail::basexx::try_decode_to<detail::basexx::identifier::base16, std::vector>
    try_decode16_to_vector{};
inline constexpr detail::basexx::try_decode_to<detail::basexx::identifier::base32, std::vector>
    try_decode32_to_vector{};
inline constexpr detail::basexx::try_decode_to<detail::basexx::identifier::base32hex, std::vector>
    try_decode32hex_to_vector{};
inline constexpr detail::basexx::try_decode_to<detail::basexx::identifier::base64, std::vector>
    try_decode64_to_vector{};

inline constexpr detail::basexx::decode_to<detail::basexx::identifier::base16, std::vector> decode16_to_vector{};
inline constexpr detail::basexx::decode_to<detail::basexx::identifier::base32, std::vector> decode32_to_vector{};
inline constexpr detail::basexx::decode_to<detail::basexx::identifier::base32hex, std::vector> decode32hex_to_vector{};
inline constexpr detail::basexx::decode_to<detail::basexx::identifier::base64, std::vector> decode64_to_vector{};
