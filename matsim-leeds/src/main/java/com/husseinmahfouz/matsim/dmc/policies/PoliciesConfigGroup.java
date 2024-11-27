package com.husseinmahfouz.matsim.dmc.policies;

import com.husseinmahfouz.matsim.dmc.policies.city_tax.CityTaxConfigGroup;
import com.husseinmahfouz.matsim.dmc.policies.city_tax.CityTaxPolicyFactory;
import com.husseinmahfouz.matsim.dmc.policies.limited_traffic_zone.LimitedTrafficZoneConfigGroup;
import com.husseinmahfouz.matsim.dmc.policies.limited_traffic_zone.LimitedTrafficZonePolicyFactory;
import com.husseinmahfouz.matsim.dmc.policies.transit_discount.TransitDiscountConfigGroup;
import com.husseinmahfouz.matsim.dmc.policies.transit_discount.TransitDiscountPolicyFactory;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigGroup;
import org.matsim.core.config.ReflectiveConfigGroup;

public class PoliciesConfigGroup extends ReflectiveConfigGroup {
	static public final String CONFIG_NAME = "eqasim:policies";

	public PoliciesConfigGroup() {
		super(CONFIG_NAME);
	}

	@Override
	public ConfigGroup createParameterSet(String type) {
		switch (type) {
			case CityTaxPolicyFactory.POLICY_NAME:
				return new CityTaxConfigGroup();
			case LimitedTrafficZonePolicyFactory.POLICY_NAME:
				return new LimitedTrafficZoneConfigGroup();
			case TransitDiscountPolicyFactory.POLICY_NAME:
				return new TransitDiscountConfigGroup();
			default:
				throw new IllegalStateException();
		}
	}

	static public PoliciesConfigGroup get(Config config) {
		return (PoliciesConfigGroup) config.getModules().get(CONFIG_NAME);
	}
}
