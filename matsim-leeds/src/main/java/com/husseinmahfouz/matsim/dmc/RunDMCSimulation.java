package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.scenario.RunInsertVehicles;
import org.eqasim.core.scenario.validation.VehiclesValidator;
import org.eqasim.core.simulation.analysis.EqasimAnalysisModule;
import org.eqasim.core.simulation.mode_choice.EqasimModeChoiceModule;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
// import com.husseinmahfouz.matsim.dmc.policies.PolicyExtension;
import org.matsim.api.core.v01.Scenario;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;
// TODO: Remove when age_years is turned to "age" in plans.xml input
import org.matsim.utils.objectattributes.attributable.Attributes;

public class RunDMCSimulation {
	static public void main(String[] args) throws ConfigurationException {
		CommandLine cmd = new CommandLine.Builder(args) //
				.requireOptions("config-path") //
				.allowPrefixes("mode-choice-parameter", "cost-parameter") //
				.build();

		LeedsConfigurator configurator = new LeedsConfigurator();
		Config config = ConfigUtils.loadConfig(cmd.getOptionStrict("config-path"));
		configurator.updateConfig(config);
		cmd.applyConfiguration(config);

		// PolicyExtension policies = new PolicyExtension();
		// policies.adaptConfiguration(config);

		Scenario scenario = ScenarioUtils.createScenario(config);
		configurator.configureScenario(scenario);
		ScenarioUtils.loadScenario(scenario);
		configurator.adjustScenario(scenario);

		// Rename "age_years" to "age" for all persons
		// TODO remove when "age_years" is turned to "age" in plans.xml input
		scenario.getPopulation().getPersons().values().forEach(person -> {
			Attributes attributes = person.getAttributes();
			Object ageYears = attributes.getAttribute("age_years");
			if (ageYears != null) {
				attributes.putAttribute("age", ageYears);
				attributes.removeAttribute("age_years");
			}
		});

		RunInsertVehicles.insertVehicles(config, scenario);
		VehiclesValidator.validate(config);

		Controler controller = new Controler(scenario);
		configurator.configureController(controller);
		controller.addOverridingModule(new EqasimAnalysisModule());
		controller.addOverridingModule(new EqasimModeChoiceModule());
		controller.addOverridingModule(new LeedsModeChoiceModule(cmd));
		// controller.addOverridingModule(policies);
		controller.run();
	}
}
