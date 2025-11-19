package com.husseinmahfouz.matsim.drt.waiting;

import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.drt.run.DrtConfigGroup;
import org.matsim.core.config.Config;
import org.matsim.core.controler.events.IterationEndsEvent;
import org.matsim.core.controler.listener.IterationEndsListener;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Singleton
public class DrtWaitingTimeProvider implements IterationEndsListener {

    private static final Logger log = LogManager.getLogger(DrtWaitingTimeProvider.class);

    // Map: mode -> (timebin -> average wait time in seconds)
    private final Map<String, Map<Integer, Double>> waitingTimesByModeAndTime = new HashMap<>();
    private final String outputDirectory;
    private final Set<String> drtModes;
    private final Map<String, Double> defaultWaitTimesByMode = new HashMap<>();

    @Inject
    public DrtWaitingTimeProvider(Config config) {
        log.info("✓ DrtWaitingTimeProvider CREATED with output directory: {}", 
             config.controller().getOutputDirectory());

        this.outputDirectory = config.controller().getOutputDirectory();

        // Auto-detect DRT modes from config
        MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(config);
        this.drtModes = multiModeDrtConfig.getModalElements().stream().map(DrtConfigGroup::getMode)
                .collect(Collectors.toSet());

        // Initialize with mode-specific maxWaitTime from config
        for (DrtConfigGroup drtConfig : multiModeDrtConfig.getModalElements()) {
            String mode = drtConfig.getMode();

            // Get (or create) the wrapper and the default constraints set
            var constraintsParams = drtConfig.addOrGetDrtOptimizationConstraintsParams();
            var defaultConstraints =
                    constraintsParams.addOrGetDefaultDrtOptimizationConstraintsSet();

            double maxWaitTime = defaultConstraints.maxWaitTime;

            // Handle NaN or infinite case (if maxWaitTime not set in config)
            if (Double.isNaN(maxWaitTime) || !Double.isFinite(maxWaitTime)) {
                maxWaitTime = 600.0; // Default 10 min
                log.warn("⚠️ maxWaitTime not set for mode {}. Using default: {} seconds", mode,
                        maxWaitTime);

            }

            defaultWaitTimesByMode.put(mode, maxWaitTime);
            waitingTimesByModeAndTime.put(mode, new HashMap<>());

            log.info("DrtWaitingTimeProvider: Mode {} initialized with maxWaitTime: {} seconds",
                    mode, maxWaitTime);

        }
    }

    @Override
    public void notifyIterationEnds(IterationEndsEvent event) {
        int iteration = event.getIteration();

        log.info("DrtWaitingTimeProvider: Processing iteration {} with output directory: {}", 
             iteration, outputDirectory);

        // Read wait stats for each DRT mode
        for (String mode : drtModes) {
            String statsFile = outputDirectory + "/ITERS/it." + iteration + "/" + iteration
                    + ".waitStats_" + mode + ".csv";

            // Log the file path being read
            log.info("Attempting to read wait stats from: {}", statsFile);


            try {
                Map<Integer, Double> timeBinnedWaits = parseWaitStats(statsFile);
                
                // Check if parsing returned any data
                if (timeBinnedWaits.isEmpty()) {
                    log.warn("⚠️ No valid wait time data found for {} in iteration {}. Using default maxWaitTime.",
                            mode, iteration);
                    continue;  // Skip to next mode
                }
                
                // Update the main map
                waitingTimesByModeAndTime.put(mode, timeBinnedWaits);

                // Calculate statistics for logging
                int totalBins = timeBinnedWaits.size();

                double avgWaitTime = timeBinnedWaits.values().stream()
                        .mapToDouble(Double::doubleValue)
                        .average()
                        .getAsDouble();  // Use getAsDouble() since we know it's not empty

                log.info(
                        "✓ Loaded wait times for {} from iteration {} (avg: {} sec across {} time bins)",
                        mode, iteration, avgWaitTime, totalBins);

            } catch (Exception e) {
                log.warn("⚠️ Could not read wait stats for {}: {}. Using default maxWaitTime.",
                        mode, e.getMessage());
            }
        }
    }

    private Map<Integer, Double> parseWaitStats(String filepath) throws Exception {
        Map<Integer, Double> timeBinnedWaits = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(filepath))) {
            String header = reader.readLine(); // Skip header
            log.info("CSV header: {}", header);  
            
            String line;
            int linesProcessed = 0;
            int validLinesStored = 0;

            while ((line = reader.readLine()) != null) {
                linesProcessed++;
                
                // Log first few lines for debugging
                if (linesProcessed <= 3) {
                    log.info("Line {}: '{}'", linesProcessed, line);
                }
                
                String[] parts = line.split(";");
                
                if (parts.length < 3) {
                    log.debug("Skipping malformed line {} (only {} parts): {}", 
                            linesProcessed, parts.length, line);
                    continue;
                }

                try {
                    String timebinStr = parts[0].trim();
                    String legsStr = parts[1].trim();
                    String avgWaitStr = parts[2].trim();
                    
                    int legs = Integer.parseInt(legsStr);
                    double avgWait = Double.parseDouble(avgWaitStr);
                    int timebinSeconds = parseTimeToSeconds(timebinStr);

                    // Only store if there were actual requests
                    if (legs > 0 && avgWait > 0) {
                        timeBinnedWaits.put(timebinSeconds, avgWait);
                        validLinesStored++;
                        if (linesProcessed <= 5) {
                            log.info("✓ Stored timebin {} ({} sec): {} wait from {} legs", 
                                    timebinStr, timebinSeconds, avgWait, legs);
                        }
                    }
                    
                } catch (NumberFormatException e) {
                    log.warn("Could not parse numbers on line {}: '{}'. Error: {}", 
                            linesProcessed, line, e.getMessage());
                }
            }
            
            log.info("Parsed {} lines from {}, stored {} valid time bins", 
                    linesProcessed, filepath, validLinesStored);
        }

        return timeBinnedWaits;
    }

    private int parseTimeToSeconds(String timeStr) {
        String[] parts = timeStr.split(":");
        int hours = Integer.parseInt(parts[0]);
        int minutes = Integer.parseInt(parts[1]);
        int seconds = Integer.parseInt(parts[2]);
        return hours * 3600 + minutes * 60 + seconds;
    }

    /**
     * Get average waiting time for a DRT mode at a specific time of day
     */
    public double getWaitingTime(String mode, double departureTime) {
        Map<Integer, Double> timeBinnedWaits = waitingTimesByModeAndTime.get(mode);

        // Case 1: No data loaded yet OR no valid time bins
        if (timeBinnedWaits == null || timeBinnedWaits.isEmpty()) {
            return defaultWaitTimesByMode.get(mode);
        }

        // Find the closest time bin with actual data
        int closestBin = findClosestTimebin(timeBinnedWaits, (int) departureTime);

        // Case 2: No valid bin found (shouldn't happen but safety check)
        if (closestBin == -1) {
            return defaultWaitTimesByMode.get(mode);
        }

        double waitTime = timeBinnedWaits.get(closestBin);

        // Case 3: Bin exists but has zero value (shouldn't happen after filtering, but safety
        // check)
        if (waitTime <= 0) {
            return defaultWaitTimesByMode.get(mode);
        }

        return waitTime;
    }

    private int findClosestTimebin(Map<Integer, Double> timeBinnedWaits, int time) {
        int closestBin = -1;
        int minDiff = Integer.MAX_VALUE;

        for (int bin : timeBinnedWaits.keySet()) {
            int diff = Math.abs(bin - time);
            
            // Update if: (1) closer, OR (2) same distance but earlier bin (e.g. for 08:00:00, prefer 07:30:00 over 08:30:00 if both are in timeBinnedWaits)
            if (diff < minDiff || (diff == minDiff && bin < closestBin)) {
                minDiff = diff;
                closestBin = bin;
            }
        }

        return closestBin;
    }
}
