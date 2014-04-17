package org.onepf.repository.model;

import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.model.services.*;

import java.util.List;

/**
 *
 * Provide download object request to underlying Data and Storage Services
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class DownloadObjectRequestHandler extends BaseRequestHandler {

    /**
     * Options object, contains following fields
     *
     * packageName - name of the package
     * fileType - type of the object to download (can be appdf or description)
     */
    public static class ObjectOptions {
        public String packageName;
        public FileType fileType = FileType.APPDF;

    }

    public DownloadObjectRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    /**
     *
     * @param options - contains description of object to download (package name and file type)
     * @return object describes file to download
     * @throws StorageException
     * @throws DataException
     *
     * @see org.onepf.repository.model.services.StorageObject
     */
    public StorageObject getObject(ObjectOptions options) throws StorageException, DataException {
        List<ApplicationEntity> appLog = dataService.getApplicationsLog(options.packageName, -1);
        String objectKey = null;
        switch (options.fileType) {
            case DESCRIPTION:
//                objectKey = appLog.get(0).descriptionLink;
                break;
            case APK:
            case APPDF:
            default:
                objectKey = appLog.get(0).getAppdfLink();
        }

        return storageService.getObject(objectKey);
    }
}
