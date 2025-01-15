package com.husseinmahfouz.matsim.dmc.mode_choice;


import java.util.Collection;
import java.util.List;

import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeAvailability;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Person;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import org.matsim.contribs.discrete_mode_choice.model.mode_availability.ModeAvailability;

public class LeedsDrtModeAvailability implements ModeAvailability{
    static public final String NAME = "LeedsDrtModeAvailability";

	private final ModeAvailability delegate = new LeedsModeAvailability();

	@Override
	public Collection<String> getAvailableModes(Person person, List<DiscreteModeChoiceTrip> trips) {
		Collection<String> modes = delegate.getAvailableModes(person, trips);

		if (modes.contains(TransportMode.walk)) {
			modes.add("drt");
		}

		return modes;
	}

    
}
