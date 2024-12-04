package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables;

import org.eqasim.core.simulation.mode_choice.utilities.variables.BaseVariables;

public class LeedsPersonVariables implements BaseVariables {
	public final boolean hasSubscription;
	public final double hhIncome;
	public final double age;
	public final boolean isStudent;
	public final boolean isMale;

	public LeedsPersonVariables(boolean hasSubscription, double hhIncome, double age,
			boolean isStudent, boolean isMale) {
		this.hasSubscription = hasSubscription;
		this.hhIncome = hhIncome;
		this.age = age;
		this.isStudent = isStudent;
		this.isMale = isMale;
	}
}
