package com.husseinmahfouz.matsim.dmc.policies.city_tax;

import com.husseinmahfouz.matsim.dmc.policies.PolicyConfigGroup;
import org.matsim.core.config.ReflectiveConfigGroup.Parameter;

public class CityTaxConfigGroup extends PolicyConfigGroup {
	public CityTaxConfigGroup() {
		super(CityTaxPolicyFactory.POLICY_NAME);
	}

	@Parameter
	public double tax_EUR = 0.0;

	@Parameter
	public String perimetersPath;
}
