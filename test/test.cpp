#include "basexx.h"

#include <cassert>
#include <sstream>

namespace {

using namespace std::string_view_literals;
using std::ranges::equal;

constexpr auto to_bytes = std::views::transform([](auto num) -> std::byte { return static_cast<std::byte>(num); });

void test_encode16()
{
	static constexpr auto encode = to_bytes | encode16;

	// RFC 4648 test vectors ("foobar" in ASCII)

	static_assert(equal(""sv | encode, ""sv));
	static_assert(equal("\x{66}"sv | encode, "66"sv));
	static_assert(equal("\x{66}\x{6F}"sv | encode, "666F"sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}"sv | encode, "666F6F"sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}"sv | encode, "666F6F62"sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | encode, "666F6F6261"sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | encode, "666F6F626172"sv));
}

void test_decode16()
{
	// RFC 4648 test vectors ("foobar" in ASCII)

	static constexpr auto decode = decode16_to_vector;

	static_assert(equal(decode(""sv), ""sv | to_bytes));
	static_assert(equal(decode("66"sv), "\x{66}"sv | to_bytes));
	static_assert(equal(decode("666F"sv), "\x{66}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("666F6F"sv), "\x{66}\x{6F}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("666F6F62"sv), "\x{66}\x{6F}\x{6F}\x{62}"sv | to_bytes));
	static_assert(equal(decode("666F6F6261"sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | to_bytes));
	static_assert(equal(decode("666F6F626172"sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | to_bytes));
}

void test_decode16_error_handling()
{
	static constexpr auto decode = try_decode16_to_vector;

	static_assert(not decode("Z123"sv).has_value() and
		      decode("Z123"sv).error() == decodexx_error::illegal_character);
	static_assert(not decode("\x{00}123"sv).has_value() and
		      decode("\x{00}123"sv).error() == decodexx_error::illegal_character);
	static_assert(not decode("\x{FF}123"sv).has_value() and
		      decode("\x{FF}123"sv).error() == decodexx_error::illegal_character);
	static_assert(not decode("A1234"sv).has_value() and
		      decode("A1234"sv).error() == decodexx_error::missing_character);
	// Note: flagging illegal padding, as opposed to illegal character, could seem strange in the case of Base16,
	// since padding is never needed in that case. But RFC 4648 specifies "=" as the general padding character, so
	// flagging its use in Base16 as illegal padding is not a violation of the standard. On the other hand, the
	// non-canonical error cannot occur in Base16, because it can only occur if actual padding is used.
	static_assert(not decode("1=34"sv).has_value() and decode("1=34"sv).error() == decodexx_error::illegal_padding);
	static_assert(not decode("12=4"sv).has_value() and decode("12=4"sv).error() == decodexx_error::illegal_padding);
}

void test_encode32()
{
	static constexpr auto encode = to_bytes | encode32;

	// RFC 4648 test vectors ("foobar" in ASCII)

	static_assert(equal(""sv | encode, ""sv));
	static_assert(equal("\x{66}"sv | encode, "MY======"sv));
	static_assert(equal("\x{66}\x{6F}"sv | encode, "MZXQ===="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}"sv | encode, "MZXW6==="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}"sv | encode, "MZXW6YQ="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | encode, "MZXW6YTB"sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | encode, "MZXW6YTBOI======"sv));
}

void test_decode32()
{
	// RFC 4648 test vectors ("foobar" in ASCII)

	static constexpr auto decode = decode32_to_vector;

	static_assert(equal(decode(""sv), ""sv | to_bytes));
	static_assert(equal(decode("MY======"sv), "\x{66}"sv | to_bytes));
	static_assert(equal(decode("MZXQ===="sv), "\x{66}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("MZXW6==="sv), "\x{66}\x{6F}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("MZXW6YQ="sv), "\x{66}\x{6F}\x{6F}\x{62}"sv | to_bytes));
	static_assert(equal(decode("MZXW6YTB"sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | to_bytes));
	static_assert(equal(decode("MZXW6YTBOI======"sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | to_bytes));
}

void test_decode32_error_handling()
{
	static constexpr auto decode = try_decode32_to_vector;

	static_assert(not decode("MYVA\x{00}LUE"sv).has_value() and
		      decode("MYVA\x{00}LUE"sv).error() == decodexx_error::illegal_character);
	static_assert(not decode("MYVA\x{FF}LUE"sv).has_value() and
		      decode("MYVA\x{FF}LUE"sv).error() == decodexx_error::illegal_character);
	static_assert(not decode("MYV\x{FF}LUE"sv).has_value() and
		      decode("MYV\x{FF}LUE"sv).error() == decodexx_error::missing_character);
	static_assert(not decode("M=YV\x{FF}LUE"sv).has_value() and
		      decode("M=YV\x{FF}LUE"sv).error() == decodexx_error::illegal_padding);
	static_assert(not decode("MY==A==="sv).has_value() and
		      decode("MY==A==="sv).error() == decodexx_error::illegal_padding);
	static_assert(not decode("MZ======"sv).has_value() and
		      decode("MZ======"sv).error() == decodexx_error::non_canonical);
	static_assert(not decode("M7======"sv).has_value() and
		      decode("M7======"sv).error() == decodexx_error::non_canonical);
	static_assert(not decode("MY======A"sv).has_value() and
		      decode("MY======A"sv).error() == decodexx_error::missing_character);
}

void test_encode32hex()
{

	static constexpr auto encode = to_bytes | encode32hex;

	// RFC 4648 test vectors ("foobar" in ASCII)

	static_assert(equal(""sv | encode, ""sv));
	static_assert(equal("\x{66}"sv | encode, "CO======"sv));
	static_assert(equal("\x{66}\x{6F}"sv | encode, "CPNG===="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}"sv | encode, "CPNMU==="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}"sv | encode, "CPNMUOG="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | encode, "CPNMUOJ1"sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | encode, "CPNMUOJ1E8======"sv));
}

void test_decode32hex()
{
	// RFC 4648 test vectors ("foobar" in ASCII)

	static constexpr auto decode = decode32hex_to_vector;

	static_assert(equal(decode(""sv), ""sv | to_bytes));
	static_assert(equal(decode("CO======"sv), "\x{66}"sv | to_bytes));
	static_assert(equal(decode("CPNG===="sv), "\x{66}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("CPNMU==="sv), "\x{66}\x{6F}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("CPNMUOG="sv), "\x{66}\x{6F}\x{6F}\x{62}"sv | to_bytes));
	static_assert(equal(decode("CPNMUOJ1"sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | to_bytes));
	static_assert(equal(decode("CPNMUOJ1E8======"sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | to_bytes));
}

void test_encode64()
{
	static constexpr auto encode = to_bytes | encode64;

	// "Man" in ASCII

	static_assert(equal("\x{4D}\x{61}\x{6E}"sv | encode, "TWFu"sv));
	static_assert(equal("\x{4D}\x{61}"sv | encode, "TWE="sv));
	static_assert(equal("\x{4D}"sv | encode, "TQ=="sv));

	// "light work." in ASCII

	static_assert(equal("\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}\x{2E}"sv | encode,
			    "bGlnaHQgd29yay4="sv));
	static_assert(
	    equal("\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}"sv | encode, "bGlnaHQgd29yaw=="sv));
	static_assert(equal("\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}"sv | encode, "bGlnaHQgd29y"sv));
	static_assert(equal("\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}"sv | encode, "bGlnaHQgd28="sv));
	static_assert(equal("\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}"sv | encode, "bGlnaHQgdw=="sv));

	// "Many hands make light work." in ASCII

	constexpr auto many_etc =
	    "\x{4D}\x{61}\x{6E}\x{79}\x{20}\x{68}\x{61}\x{6E}\x{64}\x{73}\x{20}\x{6D}\x{61}\x{6B}\x{65}\x{20}"sv
	    "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}\x{2E}"sv;
	static_assert(equal(many_etc | encode, "TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu"sv));

	// RFC 4648 test vectors ("foobar" in ASCII)

	static_assert(equal(""sv | encode, ""sv));
	static_assert(equal("\x{66}"sv | encode, "Zg=="sv));
	static_assert(equal("\x{66}\x{6F}"sv | encode, "Zm8="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}"sv | encode, "Zm9v"sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}"sv | encode, "Zm9vYg=="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | encode, "Zm9vYmE="sv));
	static_assert(equal("\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | encode, "Zm9vYmFy"sv));
}

void test_encode64_preservation_of_range_properties()
{
	static constexpr auto encode = to_bytes | encode64;

	// "Many hands make light work." in ASCII

	constexpr auto many_etc =
	    "\x{4D}\x{61}\x{6E}\x{79}\x{20}\x{68}\x{61}\x{6E}\x{64}\x{73}\x{20}\x{6D}\x{61}\x{6B}\x{65}\x{20}"sv
	    "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}\x{2E}"sv;
	static_assert(std::ranges::random_access_range<decltype(many_etc | encode)>);
	static_assert(std::ranges::sized_range<decltype(many_etc | encode)>);
	static_assert(std::ranges::size(many_etc | encode) == 36);
}

void test_decode64()
{
	static constexpr auto decode = decode64_to_vector;

	// "Man" in ASCII

	static_assert(equal(decode("TWFu"sv), "\x{4D}\x{61}\x{6E}"sv | to_bytes));
	static_assert(equal(decode("TWE="sv), "\x{4D}\x{61}"sv | to_bytes));
	static_assert(equal(decode("TQ=="sv), "\x{4D}"sv | to_bytes));

	// "light work." in ASCII

	static_assert(equal(decode("bGlnaHQgd29yay4="sv),
			    "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}\x{2E}"sv | to_bytes));
	static_assert(equal(decode("bGlnaHQgd29yaw=="sv),
			    "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}"sv | to_bytes));
	static_assert(
	    equal(decode("bGlnaHQgd29y"sv), "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}"sv | to_bytes));
	static_assert(equal(decode("bGlnaHQgd28="sv), "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("bGlnaHQgdw=="sv), "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}"sv | to_bytes));

	// "Many hands make light work." in ASCII

	constexpr auto many_etc =
	    "\x{4D}\x{61}\x{6E}\x{79}\x{20}\x{68}\x{61}\x{6E}\x{64}\x{73}\x{20}\x{6D}\x{61}\x{6B}\x{65}\x{20}"sv
	    "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}\x{2E}"sv;
	static_assert(equal(decode("TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu"sv), many_etc | to_bytes));

	// RFC 4648 test vectors ("foobar" in ASCII)

	static_assert(equal(decode(""sv), ""sv | to_bytes));
	static_assert(equal(decode("Zg=="sv), "\x{66}"sv | to_bytes));
	static_assert(equal(decode("Zm8="sv), "\x{66}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("Zm9v"sv), "\x{66}\x{6F}\x{6F}"sv | to_bytes));
	static_assert(equal(decode("Zm9vYg=="sv), "\x{66}\x{6F}\x{6F}\x{62}"sv | to_bytes));
	static_assert(equal(decode("Zm9vYmE="sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}"sv | to_bytes));
	static_assert(equal(decode("Zm9vYmFy"sv), "\x{66}\x{6F}\x{6F}\x{62}\x{61}\x{72}"sv | to_bytes));
}

void test_decode64_error_handling()
{
	static constexpr auto decode = try_decode64_to_vector;

	static_assert(not decode("TWFuT"sv).has_value() and
		      decode("TWFuT"sv).error() == decodexx_error::missing_character);
	static_assert(not decode("bGlnaHQgd2=="sv).has_value() and
		      decode("bGlnaHQgd2=="sv).error() == decodexx_error::non_canonical);
	static_assert(not decode("bGlnaH\x{00}gd28="sv).has_value() and
		      decode("bGlnaH\x{00}gd28="sv).error() == decodexx_error::illegal_character);
	static_assert(not decode("bGlnaH\x{FF}gd28="sv).has_value() and
		      decode("bGlnaH\x{FF}gd28="sv).error() == decodexx_error::illegal_character);
	static_assert(not decode("bGln=HQgd28="sv).has_value() and
		      decode("bGln=HQgd28="sv).error() == decodexx_error::illegal_padding);
}

void test_decode64_input_range()
{
	std::istringstream many_etc{"TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu"};
	auto many_etc_view = std::views::istream<char>(many_etc);
	assert(
	    equal(decode64_to_vector(many_etc_view),
		  "\x{4D}\x{61}\x{6E}\x{79}\x{20}\x{68}\x{61}\x{6E}\x{64}\x{73}\x{20}\x{6D}\x{61}\x{6B}\x{65}\x{20}"sv
		  "\x{6C}\x{69}\x{67}\x{68}\x{74}\x{20}\x{77}\x{6F}\x{72}\x{6B}\x{2E}"sv |
		      to_bytes));
}
void test_decode64_input_range_error_handling()
{
	std::istringstream missing_character{"TWFuT"};
	auto missing_character_view = std::views::istream<char>(missing_character);
	const auto missing_char_result = try_decode64_to_vector(missing_character_view);
	assert(not missing_char_result.has_value());
	assert(missing_char_result.error() == decodexx_error::missing_character);
}

void test_encode64_decode64()
{
	// Note: the try_decode functions return updated input and output iterators when they are successful. If the
	// passed range to decode is an automatic variable, the returned iterators would be dangling. In such a case,
	// even if we use the decode functions, which throw away those iterators, the decoded expression is not a
	// constant expression. In order to avoid that problem, we need to ensure that the encoded range still is in
	// scope when the decoding function returns. Hence the somewhat verbose code below.
	constexpr auto man = "\x{4D}\x{61}\x{6E}"sv;
	constexpr auto encoded = man | to_bytes | encode64;
	static_assert(equal(decode64_to_vector(encoded), man | to_bytes));
}

void test_decode64_encode64()
{
	constexpr auto encoded = "TWFu"sv;
	static_assert(equal(decode64_to_vector(encoded) | encode64, encoded));
}

} // namespace

auto main() -> int
{
	test_encode16();
	test_decode16();
	test_decode16_error_handling();
	test_encode32();
	test_decode32();
	test_decode32_error_handling();
	test_encode32hex();
	test_decode32hex();
	// Note: no explicit check for Base32 Hex error handling, since only the alphabet symbols differ from regular
	// Base32.
	test_encode64();
	test_decode64();
	test_decode64_error_handling();
	// "Special functionality" only tested for Base64 to avoid even more verbosity. Base16 and Base32 should give
	// the same results. Also worth noting: input range encoding is at the time of writing not provided, because the
	// best solution to run our algorithm without multi-pass would be to use views::cache_last between
	// views::transform and views::cartesian_product, but that facility at the time of writing is only proposed for
	// standardization (https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2024/p3138r0.html). Also, using
	// views::cache_last will not be sufficient: in order for our implementation not to be ill-formed with input
	// ranges, we will also need to implement a variant of std::transform that does not require
	// equality-preservation (see
	// https://stackoverflow.com/questions/79069912/stdviewschunk-stdviewstransform-inputs-ranges-and-ill-formedness).
	test_encode64_preservation_of_range_properties();
	test_decode64_input_range();
	test_decode64_input_range_error_handling();
	test_encode64_decode64();
	test_decode64_encode64();

	return 0;
}
