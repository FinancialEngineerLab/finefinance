// CR2.h

#ifndef CR2_H
#define CR2_H

class CR2_results {

public:

	CR2_results()
		: pv_premium_leg(0), pv_default_leg(0), cds_spread_in_bps(0) {}

	double pv_premium_leg;
	double pv_default_leg;
	double cds_spread_in_bps;
};

class CR2 {

public:

	// Defualt constructor:

	CR2() :T(1), N(4), notional(100), 
				r(0.05), h(0.01), rr(0.50){}

	// Constructor from input arguments:

	CR2(double _T, int _N, double _notional,
		double _r, double _h, double _rr)
		:T(_T), N(_N), notional(_notional),
		r(_r), h(_h), rr(_rr){}

	// A function to extract the premium leg, 
	// default leg and cds spread:

	CR2_results get_pv_premium_and_default_legs_and_cds_spread() const
	{
		return find_pv_premium_and_default_legs_and_cds_spread();
	}

private:

	double T; // maturity
	int N; // number of payments per year
	double notional; // notional
	double r; // risk free interest rate
	double h; // hazard rate
	double rr; // recovery rate
	
	// A function to estimate the premium leg, 
	// default leg and cds spread:
	
	CR2_results find_pv_premium_and_default_legs_and_cds_spread() const;
};

#endif