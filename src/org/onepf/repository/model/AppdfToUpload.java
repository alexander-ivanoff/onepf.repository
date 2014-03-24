package org.onepf.repository.model;

import org.onepf.repository.model.auth.AppstoreDescriptor;

import java.io.File;
import java.io.IOException;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class AppdfToUpload {

    public static final String APPDF_DESCRIPTION_FILE_NAME = "description.xml"; // Move to RepositoryOptions

    File targetDir;

    public abstract void processFile(File file, String developersContact, AppstoreDescriptor appstoreDescriptor) throws IOException;
}
