package com.husseinmahfouz.matsim.dmc.mode_choice;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import org.eqasim.core.components.config.EqasimConfigGroup;
import org.eqasim.core.simulation.mode_choice.AbstractEqasimExtension;
import org.eqasim.core.simulation.mode_choice.ParameterDefinition;
import org.eqasim.core.simulation.mode_choice.cost.CostModel;
import org.eqasim.core.simulation.mode_choice.parameters.ModeParameters;
import org.eqasim.core.simulation.mode_choice.tour_finder.ActivityTourFinderWithExcludedActivities;
import org.eqasim.core.simulation.modes.drt.mode_choice.DrtModeAvailabilityWrapper;
import org.eqasim.core.simulation.modes.feeder_drt.mode_choice.FeederDrtModeAvailabilityWrapper;
import org.eqasim.core.simulation.modes.feeder_drt.mode_choice.utilities.estimator.DefaultFeederDrtUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.costs.LeedsCarCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.costs.LeedsPtCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.costs.LeedsDrtCostModelFeeder;
import com.husseinmahfouz.matsim.dmc.mode_choice.costs.LeedsTaxiCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsBikeUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsCarUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsPtUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsWalkUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsDrtUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsTaxiUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsTaxiPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPtPredictor;
import org.matsim.contribs.discrete_mode_choice.components.tour_finder.ActivityTourFinder;
import org.matsim.contribs.discrete_mode_choice.modules.config.ActivityTourFinderConfigGroup;
import org.matsim.contribs.discrete_mode_choice.modules.config.DiscreteModeChoiceConfigGroup;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;

import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.google.inject.Provider;
import com.google.inject.name.Named;
import org.matsim.core.config.Config;


public class LeedsModeChoiceModule extends AbstractEqasimExtension {
	private final CommandLine commandLine;

	// public static final String MODE_AVAILABILITY_NAME = "LeedsDrtModeAvailability";
	public static final String MODE_AVAILABILITY_NAME = "FeederDrtModeAvailabilityWrapper";


	public static final String CAR_COST_MODEL_NAME = "LeedsCarCostModel";
	public static final String PT_COST_MODEL_NAME = "LeedsPtCostModel";
	// Add DRT
	public static final String DRT_COST_MODEL_NAME = "LeedsDrtCostModelFeeder";
	public static final String TAXI_COST_MODEL_NAME = "LeedsTaxiCostModel";

	public static final String CAR_ESTIMATOR_NAME = "LeedsCarUtilityEstimator";
	public static final String BIKE_ESTIMATOR_NAME = "LeedsBikeUtilityEstimator";
	public static final String WALK_ESTIMATOR_NAME = "LeedsWalkUtilityEstimator";
	public static final String PT_ESTIMATOR_NAME = "LeedsPtUtilityEstimator";
	// Add DRT
	public static final String DRT_ESTIMATOR_NAME = "LeedsDrtUtilityEstimator";
	public static final String FEEDER_DRT_ESTIMATOR_NAME = "DefaultFeederDrtUtilityEstimator";
	public static final String TAXI_ESTIMATOR_NAME = "LeedsTaxiUtilityEstimator";

	public static final String ISOLATED_OUTSIDE_TOUR_FINDER_NAME = "IsolatedOutsideTrips";

	public LeedsModeChoiceModule(CommandLine commandLine) {
		this.commandLine = commandLine;
	}

	@Override
	protected void installEqasimExtension() {
		bindModeAvailability(MODE_AVAILABILITY_NAME).to(FeederDrtModeAvailabilityWrapper.class);

		bind(LeedsPersonPredictor.class);

		bindCostModel(CAR_COST_MODEL_NAME).to(LeedsCarCostModel.class);
		bindCostModel(PT_COST_MODEL_NAME).to(LeedsPtCostModel.class);
		// Add DRT
		bindCostModel(DRT_COST_MODEL_NAME).to(LeedsDrtCostModelFeeder.class);
		bindCostModel(TAXI_COST_MODEL_NAME).to(LeedsTaxiCostModel.class);

		bindUtilityEstimator(CAR_ESTIMATOR_NAME).to(LeedsCarUtilityEstimator.class);
		bindUtilityEstimator(BIKE_ESTIMATOR_NAME).to(LeedsBikeUtilityEstimator.class);
		bindUtilityEstimator(WALK_ESTIMATOR_NAME).to(LeedsWalkUtilityEstimator.class);
		bindUtilityEstimator(PT_ESTIMATOR_NAME).to(LeedsPtUtilityEstimator.class);
		// Add DRT
		bindUtilityEstimator(DRT_ESTIMATOR_NAME).to(LeedsDrtUtilityEstimator.class);
		bindUtilityEstimator(FEEDER_DRT_ESTIMATOR_NAME).to(DefaultFeederDrtUtilityEstimator.class);


		bindUtilityEstimator(TAXI_ESTIMATOR_NAME).to(LeedsTaxiUtilityEstimator.class);
		bind(LeedsSpatialPredictor.class);
		bind(LeedsPtPredictor.class);
		bind(LeedsTaxiPredictor.class);

		bind(ModeParameters.class).to(LeedsModeParameters.class);

		bindTourFinder(ISOLATED_OUTSIDE_TOUR_FINDER_NAME)
				.to(ActivityTourFinderWithExcludedActivities.class);
	}

	@Provides
	@Singleton
	public LeedsModeParameters provideModeChoiceParameters(EqasimConfigGroup config)
			throws IOException, ConfigurationException {
		LeedsModeParameters parameters = LeedsModeParameters.buildDefault();

		if (config.getModeParametersPath() != null) {
			ParameterDefinition.applyFile(new File(config.getModeParametersPath()), parameters);
		}

		ParameterDefinition.applyCommandLine("mode-choice-parameter", commandLine, parameters);
		return parameters;
	}

	// @Provides
	// @Singleton
	// public FeederDrtModeAvailabilityWrapper provideFeederDrtModeAvailabilityWrapper(Config
	// config) {
	// return new FeederDrtModeAvailabilityWrapper(config, new LeedsDrtModeAvailability());
	// }

	// Get DRT modes and then Feeder DRT modes
	@Provides
	@Singleton
	public FeederDrtModeAvailabilityWrapper provideFeederDrtModeAvailabilityWrapper(Config config) {
		DrtModeAvailabilityWrapper drtModeAvailabilityWrapper =
				new DrtModeAvailabilityWrapper(config, new LeedsModeAvailability());
		return new FeederDrtModeAvailabilityWrapper(config, drtModeAvailabilityWrapper);
	}

	@Provides
	@Singleton
	public LeedsCostParameters provideCostParameters(EqasimConfigGroup config) {
		LeedsCostParameters parameters = LeedsCostParameters.buildDefault();

		if (config.getCostParametersPath() != null) {
			ParameterDefinition.applyFile(new File(config.getCostParametersPath()), parameters);
		}

		ParameterDefinition.applyCommandLine("cost-parameter", commandLine, parameters);
		return parameters;
	}

	@Provides
	@Singleton
	public ActivityTourFinderWithExcludedActivities provideActivityTourFinderWithExcludedActivities(
			DiscreteModeChoiceConfigGroup dmcConfig) {
		ActivityTourFinderConfigGroup config = dmcConfig.getActivityTourFinderConfigGroup();
		return new ActivityTourFinderWithExcludedActivities(List.of("outside"),
				new ActivityTourFinder(config.getActivityTypes()));
	}

	@Provides
	@Named("taxi")
	public CostModel provideTaxiCostModel(Map<String, Provider<CostModel>> factory,
			EqasimConfigGroup config) {
		return getCostModel(factory, config, "taxi");
	}
}
