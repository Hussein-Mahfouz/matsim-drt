package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.predictors.CachedVariablePredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Singleton;

@Singleton
public class LeedsSpatialPredictor extends CachedVariablePredictor<LeedsSpatialVariables> {
	@Override
	protected LeedsSpatialVariables predict(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		// boolean hasUrbanOrigin = LeedsPredictorUtils.isUrbanArea(trip.getOriginActivity());
		// boolean hasUrbanDestination =
		// LeedsPredictorUtils.isUrbanArea(trip.getDestinationActivity());
		// is this a commuting trip?
		boolean isCommuting = LeedsPredictorUtils.isCommutingTrip(trip.getOriginActivity(),
				trip.getDestinationActivity());

		return new LeedsSpatialVariables(
				// hasUrbanOrigin, hasUrbanDestination,
				isCommuting);
	}
}
