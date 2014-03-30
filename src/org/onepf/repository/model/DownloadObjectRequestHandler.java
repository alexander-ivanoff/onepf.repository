package org.onepf.repository.model;

import org.onepf.repository.model.services.*;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;

import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public class DownloadObjectRequestHandler extends BaseRequestHandler {

    public static class ObjectOptions {
        public String packageName;
        public FileType fileType = FileType.APPDF;

    }

    public DownloadObjectRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public StorageObject getObject(ObjectOptions options) throws StorageException, DataException {
        List<ApplicationDescriptor> appLog = dataService.getApplicationsLog(options.packageName, -1);
        String objectKey = null;
        switch (options.fileType) {
            case DESCRIPTION:
                objectKey = appLog.get(0).descriptionLink;
                break;
            case APK:
            case APPDF:
            default:
                objectKey = appLog.get(0).appdfLink;
        }

        return storageService.getObject(objectKey);
    }
}
