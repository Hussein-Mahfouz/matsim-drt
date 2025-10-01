package com.husseinmahfouz.matsim.pt2matsim;

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
            if (feed.isDirectory() || !feed.getName().endsWith(".zip")) continue;
            String feedName = feed.getName().replace(".zip", "");
            String feedOutputDir = outputDir + "/" + feedName;
            new File(feedOutputDir).mkdirs();

            // Step 1: Convert GTFS to MATSim transit schedule
            String scheduleFile = feedOutputDir + "/schedule_unmapped.xml";
            String vehiclesFile = feedOutputDir + "/vehicles_unmapped.xml";
            System.out.println("Converting GTFS feed: " + feed.getName());
            RunPTConverter.main(new String[]{
                feed.getAbsolutePath(),
                serviceDate,
                crs,
                scheduleFile,
                vehiclesFile
            });

            // Step 2: Prepare a config file for PTMapper with standard output names in the subfolder
            String mappedNetworkFile = feedOutputDir + "/network_mapped.xml.gz";
            String mappedScheduleFile = feedOutputDir + "/schedule_mapped.xml.gz";
            String tempConfigPath = feedOutputDir + "/pt2matsim_config.xml";

            // Copy template config to temp config
            Files.copy(new File(pt2matsimConfigTemplate).toPath(), new File(tempConfigPath).toPath(), StandardCopyOption.REPLACE_EXISTING);

            // Read and update config
            String configContent = new String(Files.readAllBytes(new File(tempConfigPath).toPath()));
            configContent = configContent.replaceAll(
                "<param name=\"inputScheduleFile\" value=\"[^\"]*\" ?/>",
                "<param name=\"inputScheduleFile\" value=\"" + scheduleFile + "\" />");
            configContent = configContent.replaceAll(
                "<param name=\"outputNetworkFile\" value=\"[^\"]*\" ?/>",
                "<param name=\"outputNetworkFile\" value=\"" + mappedNetworkFile + "\" />");
            configContent = configContent.replaceAll(
                "<param name=\"outputScheduleFile\" value=\"[^\"]*\" ?/>",
                "<param name=\"outputScheduleFile\" value=\"" + mappedScheduleFile + "\" />");
            Files.write(new File(tempConfigPath).toPath(), configContent.getBytes());

            // Step 3: Map the transit schedule to the MATSim network
            System.out.println("Mapping schedule for: " + feedName);
            RunPTMapper.main(new String[]{ tempConfigPath });
        }
        System.out.println("Batch GTFS conversion complete.");
    }
}