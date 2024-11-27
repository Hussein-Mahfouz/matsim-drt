package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables;

import org.eqasim.core.simulation.mode_choice.utilities.variables.BaseVariables;

public class IDFSpatialVariables implements BaseVariables {
	public final boolean hasUrbanOrigin;
	public final boolean hasUrbanDestination;

	public IDFSpatialVariables(boolean hasUrbanOrigin, boolean hasUrbanDestination) {
		this.hasUrbanOrigin = hasUrbanOrigin;
		this.hasUrbanDestination = hasUrbanDestination;
	}
}
