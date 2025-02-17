package com.husseinmahfouz.matsim.dmc.policies.transit_discount;

import com.husseinmahfouz.matsim.dmc.policies.PolicyConfigGroup;
import org.matsim.core.config.ReflectiveConfigGroup.Parameter;

public class TransitDiscountConfigGroup extends PolicyConfigGroup {
	public TransitDiscountConfigGroup() {
		super(TransitDiscountPolicyFactory.POLICY_NAME);
	}

	@Parameter
	public double priceFactor = 1.0;
}
