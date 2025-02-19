package com.husseinmahfouz.matsim.dmc.policies.transit_discount;

import java.util.Map;
import java.util.Objects;

import org.eqasim.core.components.config.EqasimConfigGroup;
import org.eqasim.core.simulation.mode_choice.AbstractEqasimExtension;
import org.eqasim.core.simulation.mode_choice.cost.CostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Network;

import com.google.inject.Provides;
import com.google.inject.Singleton;

public class TransitDiscountPolicyExtension extends AbstractEqasimExtension {
	@Override
	protected void installEqasimExtension() {}

	@Provides
	@Singleton
	TransitDiscountPolicyFactory provideTransitDiscountPolicyFactory(Network network,
			LeedsModeParameters modeParameters, Map<String, CostModel> costModels,
			EqasimConfigGroup eqasimConfig) {
		CostModel costModel = Objects
				.requireNonNull(costModels.get(eqasimConfig.getCostModels().get(TransportMode.pt)));
		return new TransitDiscountPolicyFactory(getConfig(), costModel, modeParameters);
	}
}
