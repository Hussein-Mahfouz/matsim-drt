package com.husseinmahfouz.matsim.dmc.calibration;

import org.eqasim.core.simulation.analysis.EqasimAnalysisModule;
import org.eqasim.core.simulation.mode_choice.EqasimModeChoiceModule;
import com.husseinmahfouz.matsim.dmc.LeedsConfigurator;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
import org.matsim.api.core.v01.Scenario;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;

public class RunDMCSimulationCalibration {

    public static void main(String[] args) throws ConfigurationException {
        // Parse command-line arguments
        CommandLine cmd = new CommandLine.Builder(args)
                .requireOptions("config-path") // Require the configuration file path
                .allowOptions("sample-size", "iterations", 
                "output-directory", "input-plans-file", "vehicles-file",
                "global-threads", "qsim-threads") //
                .allowPrefixes("mode-choice-parameter") // Allow mode choice parameters to be passed
                .build();

        // Load the configuration file
        String configPath = cmd.getOptionStrict("config-path");
        Config config = ConfigUtils.loadConfig(configPath);

        // Apply Leeds-specific configuration
        LeedsConfigurator configurator = new LeedsConfigurator();
        configurator.updateConfig(config);

        // Update config parameters based on sample size if specified
        if (cmd.hasOption("sample-size")) {
            double sampleSize = Double.parseDouble(cmd.getOptionStrict("sample-size"));

            // Update relevant configuration parameters based on the sample size
            config.qsim().setFlowCapFactor(sampleSize);
            config.qsim().setStorageCapFactor(sampleSize);
        }

        // Update the global number of threads if specified
        if (cmd.hasOption("global-threads")) {
            int globalThreads = Integer.parseInt(cmd.getOptionStrict("global-threads"));
            config.global().setNumberOfThreads(globalThreads);
        } else {
            config.global().setNumberOfThreads(8); // Default to 8 threads
        }

        // Update the QSim number of threads if specified
        if (cmd.hasOption("qsim-threads")) {
            int qsimThreads = Integer.parseInt(cmd.getOptionStrict("qsim-threads"));
            config.qsim().setNumberOfThreads(qsimThreads);
        } else {
            config.qsim().setNumberOfThreads(8); // Default to 8 threads
        }

        // Set the number of iterations if specified
        if (cmd.hasOption("iterations")) {
            int iterations = Integer.parseInt(cmd.getOptionStrict("iterations"));
            config.controller().setLastIteration(iterations);
        } else {
            // Default to 50 iterations if not specified
            config.controller().setLastIteration(50);
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

        // Apply additional command-line configurations
        cmd.applyConfiguration(config);

        // Load the scenario
        Scenario scenario = ScenarioUtils.createScenario(config);
        configurator.configureScenario(scenario);
        ScenarioUtils.loadScenario(scenario);
        configurator.adjustScenario(scenario);

        // Create the controller
        Controler controller = new Controler(scenario);
        configurator.configureController(controller);

        // Add necessary modules
        controller.addOverridingModule(new EqasimAnalysisModule());
        controller.addOverridingModule(new EqasimModeChoiceModule());
        controller.addOverridingModule(new LeedsModeChoiceModule(cmd));

        // Run the simulation
        controller.run();

        // Save the updated configuration to the output directory
        String outputDirectory = config.controller().getOutputDirectory();
        ConfigUtils.writeConfig(config, outputDirectory + "/config.xml");
    }
}