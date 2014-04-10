package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.appdf.model.Application;
import org.onepf.appdf.parser.AppdfFileParser;
import org.onepf.appdf.parser.ParseResult;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
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
 *
 * Handle appdf file uploaded to server in UploadAppdfFileServlet. Parse appdf file, check it,
 * provide data to underlying Data and Storage Services.
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class UploadAppdfRequestHandler extends BaseRequestHandler {

    public static final String APPDF_DESCRIPTION_FILE_NAME = "description.xml"; // Move to RepositoryOptions

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private static HexBinaryAdapter marshaler = new HexBinaryAdapter();

    private final Logger logger = LogManager.getLogger(UploadAppdfRequestHandler.class.getName());

    public UploadAppdfRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    /**
     *
     * Main method to process appdf file, store it to StorageService and put information about it to DataService
     *
     * @param file link to uploaded appdf file in local filesystem
     * @param developersContact some information to contact with developer (email). Not used now.
     * @param appstoreDescriptor Descriptor of the appstore uploaded appdf file
     * @throws IOException
     * @throws StorageException
     * @throws DataException
     * @throws NoSuchAlgorithmException
     */
    public void processFile(File file, String developersContact, AppstoreDescriptor appstoreDescriptor) throws IOException, StorageException, DataException, NoSuchAlgorithmException {
        long time = System.currentTimeMillis();
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
        logger.debug("Appdf file hash: {}", appdfHash);
        sendDescription(descrKey, parseResult.getFile());


        ApplicationDescriptor appDescriptor = new ApplicationDescriptor();
        appDescriptor.packageName = packageName;
        appDescriptor.lastUpdated = dateFormat.format(new Date(System.currentTimeMillis()));
        appDescriptor.developerContact = developersContact;
        appDescriptor.appdfLink = appdfKey;
        appDescriptor.descriptionLink = descrKey;
        appDescriptor.appdfHash = appdfHash;
        appDescriptor.appstoreId = appstoreDescriptor.appstoreId;

        dataService.store(appDescriptor);
        logger.debug("Store Appdf time: {} ", (System.currentTimeMillis() - time));
    }

    /**
     * Save appdf file in StorageService.
     *
     * @param appdfKey - key in StorageService.
     * @param appdfFile - link to uploaded appdf file in local filesystem.
     * @return MD5 calculated hash of stored appdf file.
     * @throws IOException
     * @throws StorageException
     * @throws NoSuchAlgorithmException
     */
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

    /**
     * Save description file in StorageService
     *
     * @param descrKey - key in StorageService
     * @param zipFile - zip archive contains description.xml file
     * @return true, if description was found in zipFile and stored to StorageService. false - otherwise.
     * @throws IOException
     * @throws StorageException
     */
    private boolean sendDescription(String descrKey, ZipFile zipFile) throws IOException, StorageException {

        Enumeration<? extends ZipEntry> entries = zipFile.entries();

        while ( entries.hasMoreElements()){
            ZipEntry elem = entries.nextElement();
            String name = elem.getName();

            if (name.equals(APPDF_DESCRIPTION_FILE_NAME)) {
                extractFile(descrKey, zipFile, elem);
                return true;
            }
        }
        return false;
    }

    /**
     * Extract file described by zipEntry from zipFile and store it in StorageService;
     *
     * @param objectKey - key in StorageService
     * @param zipFile - zip archive to extract described in zipEntry file from.
     * @param zipEntry - description of zip archived file to extract
     * @throws StorageException
     * @throws IOException
     */
    private void extractFile(String objectKey, ZipFile zipFile, ZipEntry zipEntry) throws StorageException, IOException {
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

    /**
     * search free index to append as part of key in StorageService
     *
     * @param appLog - ApplicationDescriptors of packages already stored in DataService with links to StorageService keys
     * @param packageName - name of the package
     * @return free index to generate object keys
     */
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

    /**
     *
     * Generate key to StorageService by packageNmae, fileType and index
     * @param packageName - name of the package
     * @param fileType - type of the file (appdf or xml description)
     * @param index - calculated index
     * @return generated key value
     */
    private static String generateObjectKey(String packageName, FileType fileType, int index) {
        return  packageName + '/' + fileType.addExtension(packageName + "_" + index);

    }

}
