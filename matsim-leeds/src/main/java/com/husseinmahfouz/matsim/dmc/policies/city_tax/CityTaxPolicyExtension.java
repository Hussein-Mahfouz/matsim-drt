package com.husseinmahfouz.matsim.dmc.policies.city_tax;

import org.eqasim.core.simulation.mode_choice.AbstractEqasimExtension;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import org.matsim.api.core.v01.network.Network;

import com.google.inject.Provides;
import com.google.inject.Singleton;

public class CityTaxPolicyExtension extends AbstractEqasimExtension {
	@Override
	protected void installEqasimExtension() {}

	@Provides
	@Singleton
	CityTaxPolicyFactory provideCityTaxPolicyFactory(Network network,
			LeedsModeParameters modeParameters) {
		return new CityTaxPolicyFactory(getConfig(), network, modeParameters);
	}
}
