package com.husseinmahfouz.matsim.dmc.policies.transit_discount;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eqasim.core.simulation.mode_choice.cost.CostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.policies.DefaultPolicy;
import com.husseinmahfouz.matsim.dmc.policies.PoliciesConfigGroup;
import com.husseinmahfouz.matsim.dmc.policies.Policy;
import com.husseinmahfouz.matsim.dmc.policies.PolicyFactory;
import com.husseinmahfouz.matsim.dmc.policies.PolicyPersonFilter;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigGroup;

public class TransitDiscountPolicyFactory implements PolicyFactory {
	private static final Logger logger = LogManager.getLogger(TransitDiscountPolicyFactory.class);

	static public final String POLICY_NAME = "transitDiscount";

	private final Config config;
	private final CostModel costModel;
	private final LeedsModeParameters modeParameters;

	public TransitDiscountPolicyFactory(Config config, CostModel costModel,
			LeedsModeParameters modeParameters) {
		this.config = config;
		this.costModel = costModel;
		this.modeParameters = modeParameters;
	}

	@Override
	public Policy createPolicy(String name, PolicyPersonFilter personFilter) {
		for (ConfigGroup item : PoliciesConfigGroup.get(config)
				.getParameterSets(TransitDiscountPolicyFactory.POLICY_NAME)) {
			TransitDiscountConfigGroup policyItem = (TransitDiscountConfigGroup) item;

			if (policyItem.policyName.equals(name)) {
				return createPolicy(policyItem, personFilter);
			}
		}

		throw new IllegalStateException("Configuration not found for policy " + name + " of type "
				+ TransitDiscountPolicyFactory.POLICY_NAME);
	}

	private Policy createPolicy(TransitDiscountConfigGroup discountConfig,
			PolicyPersonFilter personFilter) {
		logger.info("Creating policy " + discountConfig.policyName + " of type "
				+ TransitDiscountPolicyFactory.POLICY_NAME);
		logger.info("  Price factor: " + discountConfig.priceFactor);

		return new DefaultPolicy(null, new TransitDiscountUtilityPenalty(costModel, modeParameters,
				discountConfig.priceFactor, personFilter));
	}
}
