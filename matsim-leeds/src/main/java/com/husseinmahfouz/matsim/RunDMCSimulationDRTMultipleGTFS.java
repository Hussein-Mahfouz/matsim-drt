package com.husseinmahfouz.matsim;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.eqasim.core.components.config.EqasimConfigGroup;
import org.eqasim.core.components.transit.EqasimTransitQSimModule;
import org.eqasim.core.simulation.analysis.EqasimAnalysisModule;
import org.eqasim.core.simulation.mode_choice.EqasimModeChoiceModule;
import org.eqasim.core.simulation.modes.drt.analysis.DrtAnalysisModule;
import com.husseinmahfouz.matsim.drt.rejections.RejectionConstraint;
import com.husseinmahfouz.matsim.drt.rejections.RejectionModule;
import com.husseinmahfouz.matsim.dmc.LeedsConfigurator;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
import com.husseinmahfouz.matsim.drt.LeedsDrtModule;
import org.matsim.api.core.v01.Scenario;
import org.matsim.contrib.drt.routing.DrtRoute;
import org.matsim.contrib.drt.routing.DrtRouteFactory;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.dvrp.run.DvrpQSimComponents;
import org.matsim.contribs.discrete_mode_choice.modules.config.DiscreteModeChoiceConfigGroup;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.config.groups.ControllerConfigGroup;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;

public class RunDMCSimulationDRTMultipleGTFS {

    static public void main(String[] args) throws ConfigurationException {
        CommandLine cmd = new CommandLine.Builder(args).requireOptions("config-path")
                .allowOptions( "sample-size", "iterations",
                        // input files that change based on sample size
                        "input-plans-file", "vehicles-file",
                        // where to save run data
                        "output-directory",
                        // number of threads
                        "global-threads", "qsim-threads",
                        // drt input files
                        // "drt-vehicles-file",
                        // cleanup option (to reduce disk usage)
                        "clean-iters-at-end",
                        // transit data
                        "transit-vehicles-file", "transit-schedule-file",
                        // network data
                        "network-input-file",
                        // Individual rejection handling
                        "use-rejection-constraint", "prior-requests", "prior-rejections", "min-attempts",
                        // global rejection handling
                        "enable-rejection-penalty", "target-rejection-rate", "controller-gain")
                .allowPrefixes("mode-choice-parameter", "cost-parameter").build();

        LeedsConfigurator configurator = new LeedsConfigurator();
        Config config = ConfigUtils.loadConfig(cmd.getOptionStrict("config-path"));
        configurator.updateConfig(config);

        // Update config parameters based on sample size if specified
        if (cmd.hasOption("sample-size")) {
            double sampleSize = Double.parseDouble(cmd.getOptionStrict("sample-size"));
            config.qsim().setFlowCapFactor(sampleSize);
            config.qsim().setStorageCapFactor(sampleSize);
            EqasimConfigGroup eqasimConfig = EqasimConfigGroup.get(config);
            eqasimConfig.setSampleSize(sampleSize);
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

        // Update the number of iterations if specified
        if (cmd.hasOption("iterations")) {
            int iterations = Integer.parseInt(cmd.getOptionStrict("iterations"));
            config.controller().setLastIteration(iterations);
        }
        // Update the number of threads if specified
        if (cmd.hasOption("global-threads")) {
            int globalThreads = Integer.parseInt(cmd.getOptionStrict("global-threads"));
            config.global().setNumberOfThreads(globalThreads);
        } else {
            config.global().setNumberOfThreads(8);
        }

        // Update the qsim number of threads if specified
        if (cmd.hasOption("qsim-threads")) {
            int qsimThreads = Integer.parseInt(cmd.getOptionStrict("qsim-threads"));
            config.qsim().setNumberOfThreads(qsimThreads);
        } else {
            config.qsim().setNumberOfThreads(8);
        }

        // Dynamically update config parameters if specified
        if (cmd.hasOption("output-directory")) {
            config.controller().setOutputDirectory(cmd.getOptionStrict("output-directory"));
        }
        // Keep or delete the ITERS/ directory
        if (cmd.hasOption("clean-iters-at-end")) {
            String cleanIters = cmd.getOptionStrict("clean-iters-at-end");
            config.controller()
                    .setCleanItersAtEnd(ControllerConfigGroup.CleanIterations.valueOf(cleanIters));
        }

        if (cmd.hasOption("transit-vehicles-file")) {
            if (config.getModules().containsKey("transit")) {
                config.getModules().get("transit").addParam("vehiclesFile",
                        cmd.getOptionStrict("transit-vehicles-file"));
            }
        }
        if (cmd.hasOption("transit-schedule-file")) {
            if (config.getModules().containsKey("transit")) {
                config.getModules().get("transit").addParam("transitScheduleFile",
                        cmd.getOptionStrict("transit-schedule-file"));
            }
        }

        if (cmd.hasOption("network-input-file")) {
            config.network().setInputFile(cmd.getOptionStrict("network-input-file"));
        }

        cmd.applyConfiguration(config);

        { // Edit the DMC config module
            DiscreteModeChoiceConfigGroup dmcConfig =
                    DiscreteModeChoiceConfigGroup.getOrCreate(config);

            // Add rejection constraint if specified
            if (cmd.getOption("use-rejection-constraint").map(Boolean::parseBoolean)
                    .orElse(false)) {
                Set<String> tripConstraints = new HashSet<>(dmcConfig.getTripConstraints());
                tripConstraints.add(RejectionConstraint.NAME);
                dmcConfig.setTripConstraints(tripConstraints);
            }
        }

        Scenario scenario = ScenarioUtils.createScenario(config);
        configurator.configureScenario(scenario);

        { // Add DRT route factory (in case input population has DRT in it)
            scenario.getPopulation().getFactory().getRouteFactories()
                    .setRouteFactory(DrtRoute.class, new DrtRouteFactory());
        }

        ScenarioUtils.loadScenario(scenario);
        configurator.adjustScenario(scenario);

        Controler controller = new Controler(scenario);
        configurator.configureController(controller);
        controller.addOverridingModule(new EqasimAnalysisModule());
        controller.addOverridingModule(new EqasimModeChoiceModule());
        controller.addOverridingModule(new LeedsModeChoiceModule(cmd));

        MultiModeDrtConfigGroup multiModeDrtConfig = (MultiModeDrtConfigGroup) config.getModules()
                .get(MultiModeDrtConfigGroup.GROUP_NAME);

        { // Configure controller for DRT
            controller.configureQSimComponents(components -> {
                DvrpQSimComponents.activateAllModes(multiModeDrtConfig).configure(components);
                EqasimTransitQSimModule.configure(components, config);
            });
        }

        { // Add overrides for Leeds + DRT
            controller.addOverridingModule(new LeedsDrtModule(cmd));
            controller.addOverridingModule(new RejectionModule(Arrays.asList("drt"), cmd));
            controller.addOverridingModule(new DrtAnalysisModule());
        }
        controller.run();

        // ----- Save the updated config file to the output directory
        ControllerConfigGroup controllerConfigGroup = config.controller();
        String outputDirectory = controllerConfigGroup.getOutputDirectory();
        ConfigUtils.writeConfig(config, outputDirectory + "/config.xml");
    }
}
