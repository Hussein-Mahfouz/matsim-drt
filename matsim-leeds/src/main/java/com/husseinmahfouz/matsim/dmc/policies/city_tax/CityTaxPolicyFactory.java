package com.husseinmahfouz.matsim.dmc.policies.city_tax;

import java.io.File;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.policies.DefaultPolicy;
import com.husseinmahfouz.matsim.dmc.policies.PoliciesConfigGroup;
import com.husseinmahfouz.matsim.dmc.policies.Policy;
import com.husseinmahfouz.matsim.dmc.policies.PolicyFactory;
import com.husseinmahfouz.matsim.dmc.policies.PolicyPersonFilter;
import com.husseinmahfouz.matsim.dmc.policies.routing.FixedRoutingPenalty;
import com.husseinmahfouz.matsim.dmc.policies.routing.PolicyLinkFinder;
import com.husseinmahfouz.matsim.dmc.policies.routing.PolicyLinkFinder.Predicate;
import org.matsim.api.core.v01.IdSet;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigGroup;

public class CityTaxPolicyFactory implements PolicyFactory {
	private static final Logger logger = LogManager.getLogger(CityTaxPolicyFactory.class);

	static public final String POLICY_NAME = "cityTax";

	private final Config config;
	private final Network network;
	private final LeedsModeParameters modeParameters;

	public CityTaxPolicyFactory(Config config, Network network,
			LeedsModeParameters modeParameters) {
		this.config = config;
		this.network = network;
		this.modeParameters = modeParameters;
	}

	@Override
	public Policy createPolicy(String name, PolicyPersonFilter personFilter) {
		for (ConfigGroup item : PoliciesConfigGroup.get(config)
				.getParameterSets(CityTaxPolicyFactory.POLICY_NAME)) {
			CityTaxConfigGroup policyItem = (CityTaxConfigGroup) item;

			if (policyItem.policyName.equals(name)) {
				return createPolicy(policyItem, personFilter);
			}
		}

		throw new IllegalStateException("Configuration not found for policy " + name + " of type "
				+ CityTaxPolicyFactory.POLICY_NAME);
	}

	private Policy createPolicy(CityTaxConfigGroup enterConfig, PolicyPersonFilter personFilter) {
		logger.info("Creating policy " + enterConfig.policyName + " of type "
				+ CityTaxPolicyFactory.POLICY_NAME);
		logger.info("  Perimeters: " + enterConfig.perimetersPath);
		logger.info("  Tax level: " + enterConfig.tax_EUR + " EUR");

		IdSet<Link> linkIds = PolicyLinkFinder.create(new File(ConfigGroup
				.getInputFileURL(config.getContext(), enterConfig.perimetersPath).getPath()))
				.findLinks(network, Predicate.Entering);

		logger.info("  Affected entering links: " + linkIds.size());

		return new DefaultPolicy(new FixedRoutingPenalty(linkIds,
				calculateEnterTaxPenalty(enterConfig.tax_EUR, modeParameters), personFilter),
				new CityTaxUtilityPenalty(linkIds, modeParameters, enterConfig.tax_EUR,
						personFilter));
	}

	private double calculateEnterTaxPenalty(double enterTax_EUR, LeedsModeParameters parameters) {
		double penalty_u = enterTax_EUR * parameters.betaCost_u_MU;
		double penalty_min = penalty_u / parameters.car.betaTravelTime_u_min;
		return penalty_min * 60.0;
	}
}