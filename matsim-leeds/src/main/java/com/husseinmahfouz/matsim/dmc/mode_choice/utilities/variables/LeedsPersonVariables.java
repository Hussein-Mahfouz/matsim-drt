package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables;

import org.eqasim.core.simulation.mode_choice.utilities.variables.BaseVariables;

public class LeedsPersonVariables implements BaseVariables {
	public final boolean hasSubscription;

	public LeedsPersonVariables(boolean hasSubscription) {
		this.hasSubscription = hasSubscription;
	}
}
