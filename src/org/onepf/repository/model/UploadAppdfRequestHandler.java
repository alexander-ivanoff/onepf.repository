package org.onepf.repository.model;

import org.apache.commons.io.IOUtils;
import org.onepf.appdf.model.ApkFilesInfo;
import org.onepf.appdf.model.Application;
import org.onepf.appdf.parser.AppdfFileParser;
import org.onepf.appdf.parser.ParseResult;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.uploader.utils.AXMLPrinter;
import org.onepf.repository.api.responsewriter.descriptors.ApkDescriptor;
import org.onepf.repository.utils.uploader.utils.ManifestProcessor;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import java.io.*;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Created by ivanoff on 12.03.14.
 */
public class UploadAppdfRequestHandler extends BaseRequestHandler {

    public static final String APPDF_DESCRIPTION_FILE_NAME = "description.xml"; // Move to RepositoryOptions

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private static HexBinaryAdapter marshaler = new HexBinaryAdapter();

    public UploadAppdfRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }


    public void processFile(File file, String developersContact, AppstoreDescriptor appstoreDescriptor) throws IOException, StorageException, DataException, NoSuchAlgorithmException {

        AppdfFileParser parser = new AppdfFileParser(file);
        ParseResult parseResult = parser.parse();

        //Default application
        Application application = parseResult.getApplication();

        final String packageName = application.getPackageName();

        if (packageName == null) {
            throw new IOException("Bad AppDF file!");
        }

        List<ApplicationDescriptor> appLog = dataService.getApplicationsLog(packageName, -1);
        int freeIndex = getFreeIndex(appLog, packageName);

        String appdfKey = generateObjectKey(packageName, FileType.APPDF, freeIndex);
        String descrKey = generateObjectKey(packageName, FileType.DESCRIPTION, freeIndex);

        String appdfHash = sendAppDFFile(appdfKey, file);
        System.out.println("appdf hash: " + appdfHash);
        sendDescription(descrKey, parseResult.getFile());

        processApkFiles(file, application);

        ApplicationDescriptor appDescriptor = new ApplicationDescriptor();
        appDescriptor.packageName = packageName;
        appDescriptor.lastUpdated = dateFormat.format(new Date(System.currentTimeMillis()));
        appDescriptor.developerContact = developersContact;
        appDescriptor.appdfLink = appdfKey;
        appDescriptor.descriptionLink = descrKey;
        appDescriptor.appdfHash = appdfHash;
        appDescriptor.appstoreId = appstoreDescriptor.appstoreId;

        long time = System.currentTimeMillis();
        dataService.store(appDescriptor);
        System.out.println("Put Request to data service: " + (System.currentTimeMillis() - time)); // TODO move to Log4J
    }


    private String sendAppDFFile(String appdfKey, File appdfFile) throws IOException, StorageException, NoSuchAlgorithmException {
        String hash = null;
        InputStream zis = null;
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            zis = new FileInputStream(appdfFile);
            DigestInputStream dis = new DigestInputStream(zis, md);
            storageService.storeObject(appdfKey, dis, appdfFile.length());
            hash = marshaler.marshal(dis.getMessageDigest().digest());
        } finally {
            if (zis != null) {
                zis.close();
            }
        }
        return hash;

    }


    private boolean sendDescription(String descrKey, ZipFile zipFile) throws IOException, StorageException {

        Enumeration<? extends ZipEntry> entries = zipFile.entries();

        while (entries.hasMoreElements()) {
            ZipEntry elem = entries.nextElement();
            String name = elem.getName();

            if (name.equals(APPDF_DESCRIPTION_FILE_NAME)) {
                extractFile(descrKey, FileType.DESCRIPTION, zipFile, elem);
                return true;
            }
        }
        return false;
    }


    private void extractFile(String objectKey, FileType fileType, ZipFile zipFile, ZipEntry zipEntry) throws StorageException, IOException {
        InputStream zis = null;
        try {
            zis = zipFile.getInputStream(zipEntry);
            storageService.storeObject(objectKey, zis, zipEntry.getSize());
        } finally {
            if (zis != null) {
                zis.close();
            }
        }
    }

    private static int getFreeIndex(List<ApplicationDescriptor> appLog, String packageName) {
        int index = appLog.size();
        String objectKey = generateObjectKey(packageName, FileType.APPDF, index);
        boolean isExist = false;
        do {
            isExist = false;
            for (ApplicationDescriptor app : appLog) {
                if (app.appdfLink.equals(objectKey)) {
                    isExist = true;
                    break;
                }
            }
            if (isExist) {
                objectKey = generateObjectKey(packageName, FileType.APPDF, ++index);
            }
        } while (isExist);
        return index;
    }

    private static String generateObjectKey(String packageName, FileType fileType, int index) {
        return packageName + '/' + fileType.addExtention(packageName + "_" + index);

    }

    private void processApkFiles(File file, Application application) throws IOException {
        ApkFilesInfo filesInfo = application.getFilesInfo();
        List<ApkFilesInfo.ApkFile> apkFiles = filesInfo.getApkFiles();
        ZipFile zip = new ZipFile(file, ZipFile.OPEN_READ);
        for (ApkFilesInfo.ApkFile apk : apkFiles) {
            String innerZipFileEntryName = apk.getFileName();
            File tempFile = null;
            FileOutputStream tempOut = null;
            ZipFile innerZipFile = null;
            try {
                tempFile = File.createTempFile("tempFile" + innerZipFileEntryName, "zip");
                tempOut = new FileOutputStream(tempFile);
                IOUtils.copy(zip.getInputStream(new ZipEntry(innerZipFileEntryName)), tempOut);
                innerZipFile = new ZipFile(tempFile);
                String sha1 = "";
                try {
                    sha1 = ApkDescriptor.sha1(tempFile);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                ZipEntry manifestEntry = innerZipFile.getEntry("AndroidManifest.xml");
                String manifestXMLFromAPK = AXMLPrinter.getManifestXMLFromAPK(innerZipFile.getInputStream(manifestEntry));
                //todo builder
                ManifestProcessor manifest = new ManifestProcessor().parse(manifestXMLFromAPK);
                //todo return apk or builder
                //todo just for test purpose
                ApkDescriptor apkDescriptor = new ApkDescriptor();
                apkDescriptor.setSha1(sha1);
                apkDescriptor.setPackageName(manifest.getPackageName());
                apkDescriptor.setVersionCode(Integer.valueOf(manifest.getVersionCode()));
                try {
                    dataService.addApk(apkDescriptor);
                } catch (DataException e) {
                    e.printStackTrace();
                }

                if (!manifest.getPackageName().equals(application.getPackageName())) {
                    //todo throw an excepton
                }
                boolean hasInApps = application.getContentDescription().getIncludedActivites().isInAppBilling();
                boolean hasOIABPermission = manifest.hasPermission("org.onepf.openiab.permission.BILLING");
                if (hasInApps && !hasOIABPermission) {
                    //todo throw an exception
                }

                System.out.println("org.onepf.repository.model.UploadAppdfRequestHandler.readInnerZipFile: " + manifestXMLFromAPK);

            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                try {
                    if (zip != null)
                        zip.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                IOUtils.closeQuietly(tempOut);
                if (tempFile != null && !tempFile.delete()) {
                    //todo
                    System.out.println("Could not delete " + tempFile);
                }
                try {
                    if (innerZipFile != null)
                        innerZipFile.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

//    private void validate(List<ManifestProcessor> manifests) {
//        for (int i = 0; i < manifests.size(); i++) {
//            ManifestProcessor manifest1 = manifests.get(i);
//            for (int j = i + 1; j < manifests.size(); j++) {
//                ManifestProcessor manifest2 = manifests.get(j);
//                if (manifest1.getVersionCode().equals(manifest2.getVersionCode())) {
//                    throw new IllegalStateException();
//                }
//                List<FeatureDescriptor> features = manifest1.getFeatures();
//                for (int k = 0; k < features.size(); k++) {
//                    FeatureDescriptor feature = features.get(k);
//
//                }
//            }
//        }
//    }
}
