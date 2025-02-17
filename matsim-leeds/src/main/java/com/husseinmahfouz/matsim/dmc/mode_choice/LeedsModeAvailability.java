package com.husseinmahfouz.matsim.dmc.mode_choice;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Person;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import org.matsim.contribs.discrete_mode_choice.model.mode_availability.ModeAvailability;
import org.matsim.core.population.PersonUtils;

public class LeedsModeAvailability implements ModeAvailability {

	@Override
	public Collection<String> getAvailableModes(Person person, List<DiscreteModeChoiceTrip> trips) {
		Collection<String> modes = new HashSet<>();

		// Modes that are always available
		modes.add(TransportMode.walk);
		modes.add(TransportMode.pt);
		// modes.add(TransportMode.taxi);
		// modes.add(TransportMode.drt);
		modes.add(TransportMode.taxi);

		// Check car availability
		boolean carAvailability = true;

		Boolean hasLicence = (Boolean) person.getAttributes().getAttribute("hasLicence");
		if (!hasLicence) {
			carAvailability = false;
		}

		// currently I use "yes", "no", "some", with some being for people who aren't main driver of
		// household car
		if ("no".equals((String) person.getAttributes().getAttribute("CarAvailability"))) {
			carAvailability = false;
		}

		// No one below 17 should be driving
		Integer age = PersonUtils.getAge(person);
		if (age != null && age < 17) {
			carAvailability = false;
		}

		if (carAvailability) {
			modes.add(TransportMode.car);
		}

		// Check bike availability
		boolean bikeAvailability = true;

		if ("no".equals((String) person.getAttributes().getAttribute("BicycleAvailability"))) {
			bikeAvailability = false;
		}

		if (bikeAvailability) {
			modes.add(TransportMode.bike);
		}

		// Add special mode "outside" if applicable
		Boolean isOutside = (Boolean) person.getAttributes().getAttribute("outside");

		if (isOutside != null && isOutside) {
			modes.add("outside");
		}

		// Add special mode "car_passenger" if applicable
		Boolean isCarPassenger = (Boolean) person.getAttributes().getAttribute("isPassenger");

		if (isCarPassenger != null && isCarPassenger) {
			modes.add("car_passenger");
		}

		return modes;
	}
}
