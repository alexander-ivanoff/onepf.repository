package org.onepf.repository.uploader;

import org.apache.http.HttpEntity;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import java.io.File;
import java.io.FileFilter;
import java.util.*;

public class Main {

    private static Statistics stats = new Statistics("failed.txt");

    public static void main(String[] args) throws Exception {
        if (args.length < 1)  {
            System.out.println("File path not given");
            System.exit(1);
        }
        CloseableHttpClient httpclient = HttpClients.createDefault();


        List<File> files = listAppdfFiles(new File(args[0]), Integer.valueOf(args[1]), Integer.valueOf(args[2]));
        System.out.println("There are " + files.size() + " files to Handle");
        uploadFiles(files, "http://default-environment-h54zw34i9w.elasticbeanstalk.com/openaep/upload", "W2K423WUYFm16SvY3d0Qh6iRu6uEZ7NQ");
    }

    public static Map<File, String> uploadFiles(Collection<File> files, String url, String authToken) throws Exception{
        Map<File, String> failedToUploadWithReason = new HashMap<File, String>();

        CloseableHttpClient httpclient = HttpClients.createDefault();
        try {
            StringBody authTokenBody = new StringBody(authToken, ContentType.TEXT_PLAIN);
            HttpPost httppost = new HttpPost(url);
            for (File file : files) {
                FileBody fileBody = new FileBody(file);

                HttpEntity reqEntity = MultipartEntityBuilder.create()
                        .addPart("data", fileBody)
                        .addPart("authToken", authTokenBody)
                        .build();

                httppost.setEntity(reqEntity);
                System.out.println("executing request " + httppost.getRequestLine());
                long uploadtime = System.currentTimeMillis();
                CloseableHttpResponse response = httpclient.execute(httppost);
                uploadtime = System.currentTimeMillis() - uploadtime;
                System.out.println("Upload time: " + uploadtime);
                try {
                    System.out.println("----------------------------------------");
                    System.out.println(response.getStatusLine());
                    int resultCode = response.getStatusLine().getStatusCode();

                    if (resultCode == HttpStatus.SC_OK) {
                        HttpEntity resEntity = response.getEntity();
                        if (resEntity != null) {
                            stats.addFileStat(file.getName(), file.length(), uploadtime, true, null);
                        }
                        EntityUtils.consume(resEntity);
                    } else {
                        throw new Exception(response.getStatusLine().toString());
                    }
                } catch (Exception e) {
                    failedToUploadWithReason.put(file, e.getMessage());
                    stats.addFileStat(file.getName(), file.length(), uploadtime, false, e.getMessage());

                } finally {
                    response.close();

                }

            }
        } finally {
            httpclient.close();
            stats.calcAndPrintStats();
            stats.saveStat();
        }
        return failedToUploadWithReason;
    }

    public static List<File> listAppdfFiles(File dir, int offset, int limit) {
        System.out.print("Looking for appdf file in " + dir.getPath()
                + ", isExist = " + dir.exists()
                + ", isDirectory = " + dir.isDirectory());


        File[] files = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File pathname) {
                System.out.println("name:" + pathname.getName());
                return pathname.getName().endsWith(".appdf");
            }
        });
        int fCount = files.length;
        if (offset >= fCount) {
            throw new IllegalArgumentException("offset is out of bounds. Number of appdf files:  " + fCount);
        }
        int max = Math.min(offset + limit, fCount);

        List<File> filesToHandle = new ArrayList<File>();
        for (int i = offset; i < max; i++) {
            filesToHandle.add(files[i]);
        }
        return filesToHandle;

    }

}
