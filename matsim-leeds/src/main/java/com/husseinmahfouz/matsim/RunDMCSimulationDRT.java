package com.husseinmahfouz.matsim;

import org.eqasim.core.components.transit.EqasimTransitQSimModule;
// import org.eqasim.core.components.config.EqasimConfigGroup;
// import org.eqasim.core.components.transit.EqasimTransitQSimModule;
import org.eqasim.core.simulation.analysis.EqasimAnalysisModule;
import org.eqasim.core.simulation.mode_choice.EqasimModeChoiceModule;
import org.eqasim.core.simulation.modes.drt.analysis.DrtAnalysisModule;
// import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsDrtModeAvailability;

// import org.eqasim.examples.corsica_drt.rejections.RejectionConstraint;
// import org.eqasim.examples.corsica_drt.rejections.RejectionModule;
import com.husseinmahfouz.matsim.dmc.LeedsConfigurator;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
import com.husseinmahfouz.matsim.drt.LeedsDrtModule;
import org.matsim.api.core.v01.Scenario;
import org.matsim.contrib.dvrp.run.DvrpQSimComponents;
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
// import org.matsim.core.config.groups.QSimConfigGroup.StarttimeInterpretation;
// import org.matsim.core.config.groups.ScoringConfigGroup.ModeParams;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;

public class RunDMCSimulationDRT {

    static public void main(String[] args) throws ConfigurationException {
        CommandLine cmd = new CommandLine.Builder(args) //
                .requireOptions("config-path") //
                .allowOptions("use-rejection-constraint") //
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

        Controler controller = new Controler(scenario);
        configurator.configureController(controller);
        controller.addOverridingModule(new EqasimAnalysisModule());
        controller.addOverridingModule(new EqasimModeChoiceModule());
        controller.addOverridingModule(new LeedsModeChoiceModule(cmd));
        // controller.addOverridingModule(policies);


        { // Add overrides for Leeds + DRT
            controller.addOverridingModule(new LeedsDrtModule(cmd));
            // controller.addOverridingModule(new RejectionModule(Arrays.asList("drt")));
            controller.addOverridingModule(new DrtAnalysisModule());
        }
        controller.run();
    }
}
