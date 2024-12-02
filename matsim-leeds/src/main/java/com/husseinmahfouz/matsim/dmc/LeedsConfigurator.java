package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.simulation.EqasimConfigurator;
import com.husseinmahfouz.matsim.dmc.policies.PoliciesConfigGroup;

public class LeedsConfigurator extends EqasimConfigurator {
	public LeedsConfigurator() {
		super();

		registerConfigGroup(new PoliciesConfigGroup(), true);
	}
}
