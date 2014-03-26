package org.onepf.repository.model;

import org.onepf.appdf.model.Application;
import org.onepf.appdf.parser.AppdfFileParser;
import org.onepf.appdf.parser.ParseResult;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Created by ivanoff on 12.03.14.
 */
public class UploadAppdfRequestHandler extends BaseRequestHandler {

    public static final String APPDF_DESCRIPTION_FILE_NAME = "description.xml"; // Move to RepositoryOptions

    private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    public UploadAppdfRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }


    public void processFile(File file, String developersContact, AppstoreDescriptor appstoreDescriptor) throws IOException, StorageException, DataException {
        AppdfFileParser parser = new AppdfFileParser(file);
        ParseResult parseResult = parser.parse();

        //Default application
        Application application = parseResult.getApplication();

        final String packageName = application.getPackageName();

        sendAppDFFile(packageName, file);
        sendDescription(packageName, parseResult.getFile());


        ApplicationDescriptor appDescriptor = new ApplicationDescriptor();
        appDescriptor.packageName = packageName;
        appDescriptor.lastUpdated = dateFormat.format(new Date(System.currentTimeMillis()));
        appDescriptor.developerContact = developersContact;
        appDescriptor.appstoreId = appstoreDescriptor.appstoreId;

        long time = System.currentTimeMillis();
        dataService.store(appDescriptor);
        System.out.println("Put Request to data service: " + (System.currentTimeMillis() - time)); // TODO move to Log4J
    }


    private void sendAppDFFile(String packageName, File appdfFile) throws IOException, StorageException {
        InputStream zis = null;
        try {
            zis = new FileInputStream(appdfFile);
            storageService.storeObject(packageName, zis, FileType.APPDF, appdfFile.length());
        } finally {
            if (zis != null) {
                zis.close();
            }
        }
    }


    private boolean sendDescription(String packageName, ZipFile zipFile) throws IOException, StorageException {

        Enumeration<? extends ZipEntry> entries = zipFile.entries();

        while ( entries.hasMoreElements()){
            ZipEntry elem = entries.nextElement();
            String name = elem.getName();

            if (name.equals(APPDF_DESCRIPTION_FILE_NAME)) {
                extractFile(packageName, FileType.DESCRIPTION, zipFile, elem);
                return true;
            }
        }
        return false;
    }


    private void extractFile(String packageName, FileType fileType, ZipFile zipFile, ZipEntry zipEntry) throws StorageException, IOException {
        InputStream zis = null;
        try {
            zis = zipFile.getInputStream(zipEntry);
            storageService.storeObject(packageName, zis, fileType, zipEntry.getSize());
        } finally {
            if (zis != null) {
                zis.close();
            }
        }
    }

}
