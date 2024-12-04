package com.husseinmahfouz.matsim.dmc.mode_choice.costs;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.cost.CostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
// import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
// import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;
// import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
// import org.matsim.core.utils.geometry.CoordUtils;
import org.matsim.pt.routes.TransitPassengerRoute;
import org.matsim.pt.transitSchedule.api.TransitSchedule;

import com.google.inject.Inject;

public class LeedsPtCostModel implements CostModel {
	private final LeedsPersonPredictor personPredictor;
	private final LeedsCostParameters parameters;
	// private final LeedsSpatialPredictor spatialPredictor;
	// TODO: This should be hidden by some custom predictor
	private final TransitSchedule transitSchedule;

	@Inject
	public LeedsPtCostModel(LeedsPersonPredictor personPredictor, LeedsCostParameters parameters,
			// LeedsSpatialPredictor spatialPredictor,
			TransitSchedule transitSchedule) {
		this.personPredictor = personPredictor;
		// this.spatialPredictor = spatialPredictor;
		this.transitSchedule = transitSchedule;
		this.parameters = parameters;
	}

	private int getNumberOfBusVehicles(List<? extends PlanElement> elements) {
		int busCount = 0;
		for (PlanElement element : elements) {
			if (element instanceof Leg) {
				Leg leg = (Leg) element;
				if (leg.getMode().equals(TransportMode.pt)) {
					TransitPassengerRoute route = (TransitPassengerRoute) leg.getRoute();
					String transportMode = transitSchedule.getTransitLines().get(route.getLineId())
							.getRoutes().get(route.getRouteId()).getTransportMode();
					if (transportMode.equals("bus")) {
						busCount++;
					}
				}
			}
		}
		return busCount;
	}

	@Override
	public double calculateCost_MU(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		// I) If the person has a subscription, the price is zero!
		LeedsPersonVariables personVariables =
				personPredictor.predictVariables(person, trip, elements);

		if (personVariables.hasSubscription) {
			return 0.0;
		}

		int n_VehiclesBus = getNumberOfBusVehicles(elements);

		return n_VehiclesBus * parameters.busFare;

	}
}
