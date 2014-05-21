package org.onepf.repository.uploader;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Created by ivanoff on 12.04.14.
 */
public class Statistics {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    File statFile;

    List<FileStat> stats = new ArrayList<FileStat>();

    public static class FileStat {
        String fileName;
        long fileSize;
        long time;
        boolean success;
        String reason;
    }


    public Statistics(String statPath ) {
        statFile = new File(statPath);
    }

    public void addFileStat(String filename, long filesize, long time, boolean success, String reason) {
        System.out.println(filename + " - Ok!");
        FileStat stat = new FileStat();
        stat.fileName = filename;
        stat.fileSize = filesize;
        stat.time = time;
        stat.success = success;
        stat.reason = reason;
        stats.add(stat);
    }



    public void saveStat() throws IOException {
        PrintWriter writer = new PrintWriter(new FileWriter(statFile, true));
        String time = dateFormat.format(new Date(System.currentTimeMillis()));
        writer.println("<-------- " + time + "-------->");
        for(FileStat stat : stats) {
            writer.println(stat.fileName +"," + stat.fileSize + "," + stat.time + "," + stat.success + "," + stat.reason);
        }
        writer.flush();
        writer.close();
    }


    public void calcAndPrintStats() {
        // calc upload time stats
        long totalTime = 0;
        long totalSucceessTime = 0;
        long totalSize = 0;
        long totalSuccessSize = 0;
        long totalSuccessCount = 0;
        for (FileStat stat : stats) {
            totalTime += stat.time;
            totalSize += stat.fileSize;
            if (stat.success) {
                totalSuccessCount++;
                totalSucceessTime += stat.time;
                totalSuccessSize += stat.fileSize;
            }
        }
        System.out.println("<----- Upload Files Stat ---->");
        System.out.println("total time: "  + (totalTime));
        System.out.println("Success upload speed: "  + (totalSuccessSize / totalSucceessTime));
        System.out.println("Success time per file: "  + (totalSucceessTime / totalSuccessCount));
    }


}
