package com.husseinmahfouz.matsim.dmc.policies;

import com.husseinmahfouz.matsim.dmc.policies.mode_choice.UtilityPenalty;
import com.husseinmahfouz.matsim.dmc.policies.routing.RoutingPenalty;

public interface Policy {
	RoutingPenalty getRoutingPenalty();

	UtilityPenalty getUtilityPenalty();
}
