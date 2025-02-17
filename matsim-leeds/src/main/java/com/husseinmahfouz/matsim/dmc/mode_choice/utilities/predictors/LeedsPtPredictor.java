package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import java.util.List;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.CachedVariablePredictor;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PtPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.variables.PtVariables;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import org.matsim.pt.routes.TransitPassengerRoute;
import org.matsim.pt.transitSchedule.api.TransitRoute;
import org.matsim.pt.transitSchedule.api.TransitSchedule;
import com.google.inject.Inject;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPtVariables;

public class LeedsPtPredictor extends CachedVariablePredictor<LeedsPtVariables> {
	public final PtPredictor delegate;
	private final TransitSchedule schedule;

	@Inject
	public LeedsPtPredictor(PtPredictor delegate, TransitSchedule schedule) {
		this.delegate = delegate;
		this.schedule = schedule;
	}

	@Override
	protected LeedsPtVariables predict(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		PtVariables delegateVariables = delegate.predictVariables(person, trip, elements);

		double railTravelTime_min = 0.0;
		double busTravelTime_min = 0.0;
		double railWaitingTime_min = 0.0;
		double busWaitingTime_min = 0.0;
		double railAccessEgressTime_min = 0.0;
		double busAccessEgressTime_min = 0.0;

		// Iterate through each plan element (we use an iterator so that we can specifically access
		// legs / previous legs when iterating through elements)
		for (int i = 0; i < elements.size(); i++) {
			PlanElement element = elements.get(i);

			// Check if the element is a leg
			if (element instanceof Leg) {
				Leg leg = (Leg) element;

				// Check if the leg is a public transport leg
				if (leg.getMode().equals(TransportMode.pt)) {
					TransitPassengerRoute route = (TransitPassengerRoute) leg.getRoute();
					TransitRoute transitRoute = schedule.getTransitLines().get(route.getLineId())
							.getRoutes().get(route.getRouteId());

					// Get the departure time and travel time of the current leg
					double departureTime = leg.getDepartureTime().seconds();
					double travelTime = leg.getTravelTime().seconds();

					// Initialize arrival time at stop
					double arrivalTimeAtStop = 0.0;

					// Find the most recent previous leg to calculate arrival time at stop
					for (int j = i - 1; j >= 0; j--) {
						PlanElement prevElement = elements.get(j);
						if (prevElement instanceof Leg) {
							Leg prevLeg = (Leg) prevElement;
							arrivalTimeAtStop = prevLeg.getDepartureTime().seconds()
									+ prevLeg.getTravelTime().seconds();
							break;
						}
					}

					// Calculate waiting time for the transit vehicle
					double waitingTime = departureTime - arrivalTimeAtStop;

					// Check if the transit mode is rail
					if (transitRoute.getTransportMode().equals("rail")) {
						// Update rail travel time and waiting time
						railTravelTime_min += travelTime / 60.0;
						railWaitingTime_min += waitingTime / 60.0;

						// Calculate access/egress time for rail
						for (int j = i - 1; j >= 0; j--) {
							PlanElement prevElement = elements.get(j);
							if (prevElement instanceof Leg) {
								Leg prevLeg = (Leg) prevElement;
								if (prevLeg.getMode().equals(TransportMode.walk)
										|| prevLeg.getMode().equals("non_network_walk")) {
									railAccessEgressTime_min +=
											prevLeg.getTravelTime().seconds() / 60.0;
									break;
								}
							}
						}
					} else {
						// Update bus travel time and waiting time
						busTravelTime_min += travelTime / 60.0;
						busWaitingTime_min += waitingTime / 60.0;

						// Calculate access/egress time for bus
						for (int j = i - 1; j >= 0; j--) {
							PlanElement prevElement = elements.get(j);
							if (prevElement instanceof Leg) {
								Leg prevLeg = (Leg) prevElement;
								if (prevLeg.getMode().equals(TransportMode.walk)
										|| prevLeg.getMode().equals("non_network_walk")) {
									busAccessEgressTime_min +=
											prevLeg.getTravelTime().seconds() / 60.0;
									break;
								}
							}
						}
					}
				}
			}
		}

		// Return the calculated variables
		return new LeedsPtVariables(delegateVariables, railTravelTime_min, busTravelTime_min,
				railWaitingTime_min, busWaitingTime_min, railAccessEgressTime_min,
				busAccessEgressTime_min);
	}
}
