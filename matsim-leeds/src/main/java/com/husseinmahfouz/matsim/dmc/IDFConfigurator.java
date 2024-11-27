package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.simulation.EqasimConfigurator;
import com.husseinmahfouz.matsim.dmc.policies.PoliciesConfigGroup;

public class IDFConfigurator extends EqasimConfigurator {
	public IDFConfigurator() {
		super();

		registerConfigGroup(new PoliciesConfigGroup(), true);
	}
}
