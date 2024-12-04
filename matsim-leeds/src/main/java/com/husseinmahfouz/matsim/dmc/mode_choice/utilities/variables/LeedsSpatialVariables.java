package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables;

import org.eqasim.core.simulation.mode_choice.utilities.variables.BaseVariables;

public class LeedsSpatialVariables implements BaseVariables {
	public final boolean hasUrbanOrigin;
	public final boolean hasUrbanDestination;
	public final boolean isCommuting;

	public LeedsSpatialVariables(boolean hasUrbanOrigin, boolean hasUrbanDestination,
			boolean isCommuting) {
		this.hasUrbanOrigin = hasUrbanOrigin;
		this.hasUrbanDestination = hasUrbanDestination;
		this.isCommuting = isCommuting;
	}
}
