package com.husseinmahfouz.matsim;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.eqasim.core.components.config.EqasimConfigGroup;
// import org.eqasim.core.components.config.EqasimConfigGroup;
import org.eqasim.core.components.transit.EqasimTransitQSimModule;
// import org.eqasim.core.components.config.EqasimConfigGroup;
// import org.eqasim.core.components.transit.EqasimTransitQSimModule;
import org.eqasim.core.simulation.analysis.EqasimAnalysisModule;
import org.eqasim.core.simulation.mode_choice.EqasimModeChoiceModule;
import org.eqasim.core.simulation.modes.drt.analysis.DrtAnalysisModule;
// import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsDrtModeAvailability;

import com.husseinmahfouz.matsim.drt.rejections.RejectionConstraint;
import com.husseinmahfouz.matsim.drt.rejections.RejectionModule;
import com.husseinmahfouz.matsim.dmc.LeedsConfigurator;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
import com.husseinmahfouz.matsim.drt.LeedsDrtModule;
import org.matsim.api.core.v01.Scenario;
// import org.matsim.contrib.drt.optimizer.constraints.DefaultDrtOptimizationConstraintsSet;
// import org.matsim.contrib.drt.optimizer.insertion.DrtInsertionSearchParams;
// import org.matsim.contrib.drt.optimizer.insertion.selective.SelectiveInsertionSearchParams;
import org.matsim.contrib.drt.routing.DrtRoute;
import org.matsim.contrib.drt.routing.DrtRouteFactory;
// import org.matsim.contrib.drt.run.DrtConfigGroup;
// import org.matsim.contrib.drt.run.DrtConfigGroup.OperationalScheme;
// import org.matsim.contrib.drt.run.DrtConfigs;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.dvrp.run.DvrpQSimComponents;
import org.matsim.contribs.discrete_mode_choice.modules.config.DiscreteModeChoiceConfigGroup;
// import org.matsim.contrib.drt.optimizer.constraints.DefaultDrtOptimizationConstraintsSet;
// import org.matsim.contrib.drt.optimizer.insertion.DrtInsertionSearchParams;
// import org.matsim.contrib.drt.optimizer.insertion.selective.SelectiveInsertionSearchParams;
// import org.matsim.contrib.drt.routing.DrtRoute;
// import org.matsim.contrib.drt.routing.DrtRouteFactory;
// import org.matsim.contrib.drt.run.DrtConfigGroup;
// import org.matsim.contrib.drt.run.DrtConfigGroup.OperationalScheme;
// import org.matsim.contrib.drt.run.DrtConfigs;
// import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
// import org.matsim.contrib.dvrp.run.DvrpConfigGroup;
// import org.matsim.contrib.dvrp.run.DvrpQSimComponents;
// import org.matsim.contribs.discrete_mode_choice.modules.config.DiscreteModeChoiceConfigGroup;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.config.groups.ControllerConfigGroup;
import org.matsim.core.config.groups.QSimConfigGroup.StarttimeInterpretation;
// import org.matsim.core.config.groups.QSimConfigGroup.StarttimeInterpretation;
// import org.matsim.core.config.groups.ScoringConfigGroup.ModeParams;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;



public class RunDMCSimulationDRT {

    static public void main(String[] args) throws ConfigurationException {
        CommandLine cmd = new CommandLine.Builder(args) //
                .requireOptions("config-path") //
                .allowOptions("use-rejection-constraint", "sample-size", "iterations") //
                .allowPrefixes("mode-choice-parameter", "cost-parameter") //
                .build();

        LeedsConfigurator configurator = new LeedsConfigurator();
        Config config = ConfigUtils.loadConfig(cmd.getOptionStrict("config-path"));
        configurator.updateConfig(config);

        // MultiModeDrtConfigGroup multiModeDrtConfig = new MultiModeDrtConfigGroup();

        // { // Configure DRT
        // config.addModule(multiModeDrtConfig);

        // DrtConfigGroup drtConfig = new DrtConfigGroup();
        // drtConfig.mode = "drt";
        // drtConfig.operationalScheme = OperationalScheme.door2door;
        // drtConfig.stopDuration = 15.0;
        // DefaultDrtOptimizationConstraintsSet defaultDrtOptimizationConstraintsSet = new
        // DefaultDrtOptimizationConstraintsSet();
        // defaultDrtOptimizationConstraintsSet.maxWaitTime = 3600;
        // defaultDrtOptimizationConstraintsSet.maxTravelTimeAlpha = 3;
        // defaultDrtOptimizationConstraintsSet.maxTravelTimeBeta = 3600;
        // drtConfig.addOrGetDrtOptimizationConstraintsParams().addParameterSet(defaultDrtOptimizationConstraintsSet);
        // drtConfig.vehiclesFile = "../../../data/supply/drt/drt_fleet_1.xml";

        // DrtInsertionSearchParams searchParams = new SelectiveInsertionSearchParams();
        // drtConfig.setDrtInsertionSearchParams(searchParams);

        // multiModeDrtConfig.addParameterSet(drtConfig);
        // DrtConfigs.adjustMultiModeDrtConfig(multiModeDrtConfig, config.scoring(),
        // config.routing());

        // // Additional requirements
        // config.qsim().setStartTime(0.0);
        // config.qsim().setSimStarttimeInterpretation(StarttimeInterpretation.onlyUseStarttime);
        // }

        // Update config parameters based on sample size if specified
        if (cmd.hasOption("sample-size")) {

            // Get the sample size from the command line arguments
            double sampleSize = Double.parseDouble(cmd.getOptionStrict("sample-size"));


            // update the relevant config parameters based on the sample size
            // Qsim
            config.qsim().setFlowCapFactor(sampleSize);
            config.qsim().setStorageCapFactor(sampleSize);
            // Eqasim
            EqasimConfigGroup eqasimConfig = EqasimConfigGroup.get(config);
            eqasimConfig.setSampleSize(sampleSize);

        }

        // Update the number of iterations if specified
        if (cmd.hasOption("iterations")) {
            int iterations = Integer.parseInt(cmd.getOptionStrict("iterations"));
            config.controller().setLastIteration(iterations);
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

        // PolicyExtension policies = new PolicyExtension();
        // policies.adaptConfiguration(config);

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
        // controller.addOverridingModule(policies);

        MultiModeDrtConfigGroup multiModeDrtConfig = (MultiModeDrtConfigGroup) config.getModules()
                .get(MultiModeDrtConfigGroup.GROUP_NAME);

        { // Configure controller for DRT
            controller.configureQSimComponents(components -> {
                DvrpQSimComponents.activateAllModes(multiModeDrtConfig).configure(components);

                // Need to re-do this as now it is combined with DRT
                EqasimTransitQSimModule.configure(components, config);
            });
        }

        { // Add overrides for Leeds + DRT
            controller.addOverridingModule(new LeedsDrtModule(cmd));
            controller.addOverridingModule(new RejectionModule(Arrays.asList("drt")));
            controller.addOverridingModule(new DrtAnalysisModule());
        }
        controller.run();

        // ----- Save the updated config file to the output directory

        // 1. Retrieve the ControlerConfigGroup
        ControllerConfigGroup controllerConfigGroup = config.controller();
        // 2. Get the output directory
        String outputDirectory = controllerConfigGroup.getOutputDirectory();
        // 3. Save the configuration to the output directory
        ConfigUtils.writeConfig(config, outputDirectory + "/config.xml");
    }
}
