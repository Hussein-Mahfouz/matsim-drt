package com.husseinmahfouz.matsim.dmc.scenario;

import org.eqasim.core.components.config.ConfigAdapter;
import org.eqasim.core.components.config.EqasimConfigGroup;
import com.husseinmahfouz.matsim.dmc.LeedsConfigurator;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.contribs.discrete_mode_choice.modules.config.DiscreteModeChoiceConfigGroup;
import org.matsim.core.config.CommandLine.ConfigurationException;
import org.matsim.core.config.Config;
import org.matsim.core.config.groups.QSimConfigGroup;
import org.matsim.core.config.groups.QSimConfigGroup.VehiclesSource;
import org.matsim.core.config.groups.VehiclesConfigGroup;

public class RunAdaptConfig {
	static public void main(String[] args) throws ConfigurationException {
		ConfigAdapter.run(args, new LeedsConfigurator(), RunAdaptConfig::adaptConfiguration);
	}

	static public void adaptConfiguration(Config config, String prefix) {
		// Adjust eqasim config
		EqasimConfigGroup eqasimConfig = EqasimConfigGroup.get(config);

		eqasimConfig.setCostModel(TransportMode.car, LeedsModeChoiceModule.CAR_COST_MODEL_NAME);
		eqasimConfig.setCostModel(TransportMode.pt, LeedsModeChoiceModule.PT_COST_MODEL_NAME);

		eqasimConfig.setEstimator(TransportMode.car, LeedsModeChoiceModule.CAR_ESTIMATOR_NAME);
		eqasimConfig.setEstimator(TransportMode.bike, LeedsModeChoiceModule.BIKE_ESTIMATOR_NAME);

		DiscreteModeChoiceConfigGroup dmcConfig = (DiscreteModeChoiceConfigGroup) config
				.getModules().get(DiscreteModeChoiceConfigGroup.GROUP_NAME);

		dmcConfig.setModeAvailability(LeedsModeChoiceModule.MODE_AVAILABILITY_NAME);

		// Calibration results for 5%

		if (eqasimConfig.getSampleSize() == 0.05) {
			// Adjust flow and storage capacity
			config.qsim().setFlowCapFactor(0.045);
			config.qsim().setStorageCapFactor(0.045);
		}

		// Vehicles
		QSimConfigGroup qsimConfig = config.qsim();
		qsimConfig.setVehiclesSource(VehiclesSource.fromVehiclesData);

		VehiclesConfigGroup vehiclesConfig = config.vehicles();
		vehiclesConfig.setVehiclesFile(prefix + "vehicles.xml.gz");
	}
}
