package com.husseinmahfouz.matsim.pt2matsim;

import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.pt2matsim.config.PublicTransitMappingConfigGroup;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Objects;

public class RunBatchGTFSConverter {
    public static void main(String[] args) throws Exception {
        // Arguments:
        // PTConverter: [0]=GTFS dir, [1]=service date, [2]=CRS, [3]=output dir,
        // PTMapper: [4]=PT2MATSim config template
        String gtfsDirPath = args[0];
        String serviceDate = args[1];
        String crs = args[2];
        String outputDir = args[3];
        String pt2matsimConfigTemplate = args[4];

        File gtfsDir = new File(gtfsDirPath);
        if (!gtfsDir.exists() || !gtfsDir.isDirectory()) {
            System.err.println("GTFS directory not found: " + gtfsDirPath);
            System.exit(1);
        }
        new File(outputDir).mkdirs();

        for (File feed : Objects.requireNonNull(gtfsDir.listFiles())) {
            if (feed.isDirectory() || !feed.getName().endsWith(".zip"))
                continue;
            String feedName = feed.getName().replace(".zip", "").replace("_gtfs", "");
            String feedOutputDir = outputDir + "/" + feedName;
            new File(feedOutputDir).mkdirs();

            // Step 1: Convert GTFS to MATSim transit schedule
            String scheduleFile = feedOutputDir + "/schedule_unmapped.xml";
            String vehiclesFile = feedOutputDir + "/vehicles_unmapped.xml";
            System.out.println("Converting GTFS feed: " + feed.getName());
            RunPTConverter.main(new String[] {feed.getAbsolutePath(), serviceDate, crs,
                    scheduleFile, vehiclesFile});

            // Step 2: Update PT2MATSim config - Prepare a config file for PTMapper with standard
            // output names in the subfolder
            String mappedNetworkFile = feedOutputDir + "/network_mapped.xml.gz";
            String mappedScheduleFile = feedOutputDir + "/schedule_mapped.xml.gz";
            String tempConfigPath = feedOutputDir + "/pt2matsim_config.xml";

            Files.copy(new File(pt2matsimConfigTemplate).toPath(),
                    new File(tempConfigPath).toPath(), StandardCopyOption.REPLACE_EXISTING);

            // Load config, modify, and save
            Config tempConfig = ConfigUtils.loadConfig(tempConfigPath);
            PublicTransitMappingConfigGroup ptmConfig =
                    ConfigUtils.addOrGetModule(tempConfig, PublicTransitMappingConfigGroup.class);

            ptmConfig.setInputScheduleFile(scheduleFile);
            ptmConfig.setOutputNetworkFile(mappedNetworkFile);
            ptmConfig.setOutputScheduleFile(mappedScheduleFile);

            ConfigUtils.writeConfig(tempConfig, tempConfigPath);

            // Step 3: Map the transit schedule to the MATSim network
            System.out.println("Mapping schedule for: " + feedName);
            RunPTMapper.main(new String[] {tempConfigPath});
        }
        System.out.println("Batch GTFS conversion complete.");
    }
}
