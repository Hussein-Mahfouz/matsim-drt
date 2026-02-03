package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.simulation.analysis.EqasimAnalysisModule;
import org.eqasim.core.simulation.mode_choice.EqasimModeChoiceModule;
import org.eqasim.core.components.config.EqasimConfigGroup;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
import org.matsim.api.core.v01.Scenario;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;

public class RunDMCSimulation {
    static public void main(String[] args) throws ConfigurationException {
        CommandLine cmd = new CommandLine.Builder(args) //
                .requireOptions("config-path") //
                .allowOptions("sample-size", "output-directory",
                        "input-plans-file", "vehicles-file", "global-threads", "qsim-threads", "iterations") //
                .allowPrefixes("mode-choice-parameter", "cost-parameter") //
                .build();

        LeedsConfigurator configurator = new LeedsConfigurator();
        Config config = ConfigUtils.loadConfig(cmd.getOptionStrict("config-path"));
        configurator.updateConfig(config);

        // Update config parameters based on sample size if specified
        if (cmd.hasOption("sample-size")) {
            double sampleSize = Double.parseDouble(cmd.getOptionStrict("sample-size"));

            // Update QSim parameters
            config.qsim().setFlowCapFactor(sampleSize);
            config.qsim().setStorageCapFactor(sampleSize);

            // Update Eqasim parameters
            EqasimConfigGroup eqasimConfig = EqasimConfigGroup.get(config);
            eqasimConfig.setSampleSize(sampleSize);
        }

        // Update the output directory if specified
        if (cmd.hasOption("output-directory")) {
            String outputDirectory = cmd.getOptionStrict("output-directory");
            config.controller().setOutputDirectory(outputDirectory);
        }

        // Update the input plans file if specified
        if (cmd.hasOption("input-plans-file")) {
            String inputPlansFile = cmd.getOptionStrict("input-plans-file");
            config.plans().setInputFile(inputPlansFile);
        }

        // Update the vehicles file if specified
        if (cmd.hasOption("vehicles-file")) {
            String vehiclesFile = cmd.getOptionStrict("vehicles-file");
            config.vehicles().setVehiclesFile(vehiclesFile);
        }

        // Update the global number of threads if specified
        if (cmd.hasOption("global-threads")) {
            int globalThreads = Integer.parseInt(cmd.getOptionStrict("global-threads"));
            config.global().setNumberOfThreads(globalThreads);
        } else {
            config.global().setNumberOfThreads(8);
        }

        // Update the QSim number of threads if specified
        if (cmd.hasOption("qsim-threads")) {
            int qsimThreads = Integer.parseInt(cmd.getOptionStrict("qsim-threads"));
            config.qsim().setNumberOfThreads(qsimThreads);
        } else {
            config.qsim().setNumberOfThreads(8);
        }

        // Update the number of iterations if specified
        if (cmd.hasOption("iterations")) {
            int iterations = Integer.parseInt(cmd.getOptionStrict("iterations"));
            config.controller().setLastIteration(iterations);
        }

        cmd.applyConfiguration(config);

        Scenario scenario = ScenarioUtils.createScenario(config);
        configurator.configureScenario(scenario);
        ScenarioUtils.loadScenario(scenario);
        configurator.adjustScenario(scenario);

        Controler controller = new Controler(scenario);
        configurator.configureController(controller);
        controller.addOverridingModule(new EqasimAnalysisModule());
        controller.addOverridingModule(new EqasimModeChoiceModule());
        controller.addOverridingModule(new LeedsModeChoiceModule(cmd));
        controller.run();
    }
}