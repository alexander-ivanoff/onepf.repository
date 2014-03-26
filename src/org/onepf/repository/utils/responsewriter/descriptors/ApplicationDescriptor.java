package org.onepf.repository.utils.responsewriter.descriptors;

import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.Writable;
import org.onepf.repository.utils.responsewriter.WriteException;

/**
 * Created by ivanoff on 12.03.14.
 */
public class ApplicationDescriptor implements Writable {
    public String packageName;
    public String lastUpdated;
    public String version;
    public int build;
    public String developerContact;
    public String appstoreId;

    @Override
    public void write(ResponseWriter responseWriter) throws WriteException {
        responseWriter.write(this);
    }
}
