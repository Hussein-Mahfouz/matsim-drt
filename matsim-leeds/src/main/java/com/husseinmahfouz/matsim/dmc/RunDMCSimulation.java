package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.simulation.analysis.EqasimAnalysisModule;
import org.eqasim.core.simulation.mode_choice.EqasimModeChoiceModule;
import org.eqasim.core.components.config.EqasimConfigGroup;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModuleBasic;
import org.matsim.api.core.v01.Scenario;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.CommandLine.ConfigurationException;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.pt.config.TransitConfigGroup;

import java.util.Map;

public class RunDMCSimulation {

    // ── Day-of-week → representative date mapping ─────────────────────────
    // All dates are term-time week of 21-27 Sep 2026
    private static final Map<String, String> DAY_DATES = Map.of(
        "monday",    "20260921",
        "tuesday",   "20260922",
        "wednesday", "20260923",
        "thursday",  "20260924",
        "friday",    "20260925",
        "saturday",  "20260926",
        "sunday",    "20260927"
    );

    // ── Schedule file template ─────────────────────────────────────────────
    // {day} will be replaced with the day name (e.g. monday, saturday)
    private static final String SCHEDULE_TEMPLATE =
        "data/supply/schedule_{day}_mapped.xml.gz";
    private static final String NETWORK_TEMPLATE =
        "data/supply/network_{day}_mapped.xml.gz";

    static public void main(String[] args) throws ConfigurationException {
        CommandLine cmd = new CommandLine.Builder(args)
                .requireOptions("config-path")
                .allowOptions("sample-size", "output-directory",
                        "input-plans-file", "vehicles-file",
                        "global-threads", "qsim-threads", "iterations",
                        "day")   // ← NEW: day-of-week option
                .allowPrefixes("mode-choice-parameter", "cost-parameter")
                .build();

        LeedsConfigurator configurator = new LeedsConfigurator();
        Config config = ConfigUtils.loadConfig(cmd.getOptionStrict("config-path"));
        configurator.updateConfig(config);

        // ── NEW: --day option ─────────────────────────────────────────────
        // Usage: --day monday  (or tuesday, wednesday, thursday,
        //                        friday, saturday, sunday)
        // Overrides the transitScheduleFile and network in the config
        if (cmd.hasOption("day")) {
            String day = cmd.getOptionStrict("day").toLowerCase().trim();

            if (!DAY_DATES.containsKey(day)) {
                throw new ConfigurationException(
                    "Invalid --day value: '" + day + "'. " +
                    "Must be one of: monday, tuesday, wednesday, thursday, " +
                    "friday, saturday, sunday"
                );
            }

            String scheduleFile = SCHEDULE_TEMPLATE.replace("{day}", day);
            String networkFile  = NETWORK_TEMPLATE.replace("{day}", day);

            // check files exist
            java.io.File sf = new java.io.File("src/main/resources/" + scheduleFile);
            java.io.File nf = new java.io.File("src/main/resources/" + networkFile);

            if (!sf.exists()) {
                throw new ConfigurationException(
                    "Schedule file not found for day '" + day + "': " + scheduleFile + "\n" +
                    "Run first: python PyFiles/run_pt_schedule.py --day " + day
                );
            }
            if (!nf.exists()) {
                throw new ConfigurationException(
                    "Network file not found for day '" + day + "': " + networkFile + "\n" +
                    "Run first: python PyFiles/run_pt_schedule.py --day " + day
                );
            }

            // apply to config
            config.transit().setTransitScheduleFile(scheduleFile);
            config.network().setInputFile(networkFile);

            System.out.println("========================================");
            System.out.println("  Day selected : " + day.toUpperCase());
            System.out.println("  Date         : " + DAY_DATES.get(day));
            System.out.println("  Schedule     : " + scheduleFile);
            System.out.println("  Network      : " + networkFile);
            System.out.println("========================================");
        }
        // ── END NEW ───────────────────────────────────────────────────────

        // Update config parameters based on sample size if specified
        if (cmd.hasOption("sample-size")) {
            double sampleSize = Double.parseDouble(cmd.getOptionStrict("sample-size"));
            config.qsim().setFlowCapFactor(sampleSize);
            config.qsim().setStorageCapFactor(sampleSize);
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
        controller.addOverridingModule(new LeedsModeChoiceModuleBasic(cmd));
        controller.run();
    }
}
