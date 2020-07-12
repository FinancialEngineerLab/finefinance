#pragma once
#include "option.h"

class BinaryOption : public Option
{
public:
	BinaryOption(Date expiration, double strike, OptionType type) : Option(expiration, strike, type) {}
	double price();
};
