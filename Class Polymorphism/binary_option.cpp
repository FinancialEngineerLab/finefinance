#include "binary_option.h"
#include "normal.h"
#include <cmath>

double BinaryOption::price()
{
	double d2 = getd2();
	double nd2 = normcdf(type_ * d2);
	return exp(-r_ * t_) * nd2;
}