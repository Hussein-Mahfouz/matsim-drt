package com.husseinmahfouz.matsim.dmc.mode_choice;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.eqasim.core.components.config.EqasimConfigGroup;
import org.eqasim.core.simulation.mode_choice.AbstractEqasimExtension;
import org.eqasim.core.simulation.mode_choice.ParameterDefinition;
import org.eqasim.core.simulation.mode_choice.parameters.ModeParameters;
import org.eqasim.core.simulation.mode_choice.tour_finder.ActivityTourFinderWithExcludedActivities;
import com.husseinmahfouz.matsim.dmc.mode_choice.costs.LeedsCarCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.costs.LeedsPtCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsBikeUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsCarUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsPTUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsWalkUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPtPredictor;
import org.matsim.contribs.discrete_mode_choice.components.tour_finder.ActivityTourFinder;
import org.matsim.contribs.discrete_mode_choice.modules.config.ActivityTourFinderConfigGroup;
import org.matsim.contribs.discrete_mode_choice.modules.config.DiscreteModeChoiceConfigGroup;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;

import com.google.inject.Provides;
import com.google.inject.Singleton;

public class LeedsModeChoiceModule extends AbstractEqasimExtension {
	private final CommandLine commandLine;

	public static final String MODE_AVAILABILITY_NAME = "LeedsModeAvailability";

	public static final String CAR_COST_MODEL_NAME = "LeedsCarCostModel";
	public static final String PT_COST_MODEL_NAME = "LeedsPtCostModel";

	public static final String CAR_ESTIMATOR_NAME = "LeedsCarUtilityEstimator";
	public static final String BIKE_ESTIMATOR_NAME = "LeedsBikeUtilityEstimator";
	public static final String WALK_ESTIMATOR_NAME = "LeedsWalkUtilityEstimator";
	public static final String PT_ESTIMATOR_NAME = "LeedsPTUtilityEstimator";

	public static final String ISOLATED_OUTSIDE_TOUR_FINDER_NAME = "IsolatedOutsideTrips";

	public LeedsModeChoiceModule(CommandLine commandLine) {
		this.commandLine = commandLine;
	}

	@Override
	protected void installEqasimExtension() {
		bindModeAvailability(MODE_AVAILABILITY_NAME).to(LeedsModeAvailability.class);

		bind(LeedsPersonPredictor.class);

		bindCostModel(CAR_COST_MODEL_NAME).to(LeedsCarCostModel.class);
		bindCostModel(PT_COST_MODEL_NAME).to(LeedsPtCostModel.class);

		bindUtilityEstimator(CAR_ESTIMATOR_NAME).to(LeedsCarUtilityEstimator.class);
		bindUtilityEstimator(BIKE_ESTIMATOR_NAME).to(LeedsBikeUtilityEstimator.class);
		bindUtilityEstimator(WALK_ESTIMATOR_NAME).to(LeedsWalkUtilityEstimator.class);
		bindUtilityEstimator(PT_ESTIMATOR_NAME).to(LeedsPTUtilityEstimator.class);
		bind(LeedsSpatialPredictor.class);
		bind(LeedsPtPredictor.class);


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
}
