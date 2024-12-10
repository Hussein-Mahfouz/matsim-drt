package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables;

import org.eqasim.core.simulation.mode_choice.utilities.variables.BaseVariables;

public class LeedsPersonVariables implements BaseVariables {
	public final boolean hasSubscription;
	public final double hhlIncome;
	public final int age;
	public final boolean isStudent;
	public final boolean isMale;
	public final boolean isPassenger;
	public final boolean hasLicence;
	public final String carAvailability;
	public final String bikeAvailability;

	public LeedsPersonVariables(boolean hasSubscription, double hhlIncome, int age,
			boolean isStudent, boolean isMale, boolean isPassenger, boolean hasLicence,
			String carAvailability, String bikeAvailability) {
		this.hasSubscription = hasSubscription;
		this.hhlIncome = hhlIncome;
		this.age = age;
		this.isStudent = isStudent;
		this.isMale = isMale;
		this.isPassenger = isPassenger;
		this.hasLicence = hasLicence;
		this.carAvailability = carAvailability;
		this.bikeAvailability = bikeAvailability;
	}
}
