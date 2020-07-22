#include "CR2.h"
#include <iostream>
using namespace std;

int main() {
	cout << "\n *** START : Credit default Swap *** \n";

	auto T = 5;
	auto N = 20;
	auto notional = 100000000000;
	auto r = 0.013;
	auto h = 0.005;
	auto rr = 0.4; // recoveray rate

	CR2 cr2(T, N, notional, r, h, rr);

	"get_premium()";

	auto cr2_results = cr2.get_pv_premium_and_default_legs_and_cds_spread();

	//

	cout << "\n PV premium leg =" << cr2_results.pv_premium_leg << "\n";
	cout << "\n PV default leg =" << cr2_results.pv_default_leg << "\n";
	cout << "\n CDS Spread =" << cr2_results.cds_spread_in_bps << " bps \n";
	cout << "\n *** End : Credit Default Swap *** \n";
	return 0;

}