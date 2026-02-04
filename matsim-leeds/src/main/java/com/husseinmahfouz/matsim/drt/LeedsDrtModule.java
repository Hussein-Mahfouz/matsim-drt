package com.husseinmahfouz.matsim.drt;

import java.io.File;
import java.util.Map;

import org.eqasim.core.analysis.PersonAnalysisFilter;
import org.eqasim.core.components.config.EqasimConfigGroup;
import org.eqasim.core.simulation.mode_choice.AbstractEqasimExtension;
import org.eqasim.core.simulation.mode_choice.ParameterDefinition;
import org.eqasim.core.simulation.mode_choice.cost.CostModel;
import org.eqasim.core.simulation.modes.drt.mode_choice.predictors.DefaultDrtPredictor;
import org.eqasim.core.simulation.modes.drt.mode_choice.predictors.DrtPredictor;
import org.eqasim.core.simulation.modes.feeder_drt.mode_choice.utilities.estimator.DefaultFeederDrtUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsDrtModeAvailability;
import com.husseinmahfouz.matsim.dmc.mode_choice.costs.LeedsDrtCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;
// import org.eqasim.core.simulation.modes.drt.mode_choice.utilities.estimators.DrtUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsDrtPredictor;
import com.husseinmahfouz.matsim.drt.waiting.DrtWaitingTimeProvider;
import com.husseinmahfouz.matsim.drt.rejections.DrtPenaltyController;
import com.husseinmahfouz.matsim.drt.rejections.DrtPenaltyConfig;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsDrtUtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators.LeedsFeederDrtUtilityEstimator;
import org.matsim.contrib.drt.estimator.DrtEstimator;
import org.matsim.contrib.drt.optimizer.DrtOptimizer;
import org.matsim.core.config.CommandLine;

import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.google.inject.multibindings.MapBinder;
import com.google.inject.name.Named;

public class LeedsDrtModule extends AbstractEqasimExtension {
	private final CommandLine commandLine;

	public LeedsDrtModule(CommandLine commandLine) {
		this.commandLine = commandLine;
	}

	@Override
	protected void installEqasimExtension() {
		// Configure mode availability
		// bindModeAvailability(LeedsDrtModeAvailability.NAME).to(LeedsDrtModeAvailability.class);

		// Configure choice alternative for DRT
		bindUtilityEstimator("drt").to(LeedsDrtUtilityEstimator.class);
		bindCostModel("drt").to(LeedsDrtCostModel.class);

		bind(LeedsDrtPredictor.class);

		bind(DrtWaitingTimeProvider.class).asEagerSingleton();
		addControlerListenerBinding().to(DrtWaitingTimeProvider.class);
        
		bind(DrtPenaltyController.class).asEagerSingleton();
        addControlerListenerBinding().to(DrtPenaltyController.class);


		bindUtilityEstimator("feederDrt").to(LeedsFeederDrtUtilityEstimator.class);


		// Define filter for trip analysis
		// bind(PersonAnalysisFilter.class).to(DrtPersonAnalysisFilter.class);
		// my edit for now (Hussein)
		bind(PersonAnalysisFilter.class).to(PersonAnalysisFilter.class);

	}

	@Provides
	@Singleton
	public LeedsDrtCostModel provideDrtCostModel(LeedsCostParameters parameters) {
		return new LeedsDrtCostModel(parameters);
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
	@Named("drt")
	public CostModel provideCarCostModel(Map<String, Provider<CostModel>> factory,
			EqasimConfigGroup config) {
		return getCostModel(factory, config, "drt");
	}

	@Provides
    @Singleton
    public DrtPenaltyConfig provideDrtPenaltyConfig() {
        DrtPenaltyConfig config = new DrtPenaltyConfig();
        
        try {
			// Check if penalty controller should be enabled
			if (commandLine != null && commandLine.hasOption("enable-rejection-penalty")) {
				boolean enabled = Boolean.parseBoolean(
					commandLine.getOptionStrict("enable-rejection-penalty"));
				config.setEnabled(enabled);
			}
            // Only parse other parameters if enabled
			if (config.isEnabled()) {
				if (commandLine != null && commandLine.hasOption("target-rejection-rate")) {
					config.setTargetRejectionRate(
						Double.parseDouble(commandLine.getOptionStrict("target-rejection-rate")));
				}
				
				if (commandLine != null && commandLine.hasOption("controller-gain")) {
					config.setControllerGain(
						Double.parseDouble(commandLine.getOptionStrict("controller-gain")));
				}
			}
        } catch (Exception e) {
            System.err.println("Warning: Could not parse DRT penalty parameters: " + e.getMessage());
        }
        
        return config;
    }

}
