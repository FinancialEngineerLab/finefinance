#include "CR2.h"
#include <vector>
#include <cmath>
using namespace std;

CR2_results CR2::find_pv_premium_and_default_legs_and_cds_spread() const
{
	auto pv_premium_leg = 0.0;
	auto pv_default_leg = 0.0;
	auto t = 0.0;
	auto cds_spread = 0.0;
	auto array_size = static_cast<int>(N * T + 1);
	
	vector <double> DF(array_size);
	vector <double> p(array_size);

	p[0] = 1.0;

	auto dt = T / N;

	for (int j = 1; j < array_size; j++) {
		t = j * dt;
		DF[j] = exp(-r * t);
		p[j] = exp(-h * t);

		pv_premium_leg = pv_premium_leg + DF[j] * notional * dt * p[j];
		pv_default_leg = pv_default_leg + DF[j] * (1.0 - rr) * notional * (p[j - 1] - p[j]);

		cds_spread = pv_default_leg / pv_premium_leg;

		CR2_results results;
		results.pv_premium_leg = pv_premium_leg;
		results.pv_default_leg = pv_default_leg;
		results.cds_spread_in_bps = cds_spread * 10000;
		return results;
	}
}