package org.onepf.repository.utils.responsewriter.descriptors;

import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.Writable;
import org.onepf.repository.utils.responsewriter.WriteException;

/**
 * Created by ivanoff on 12.03.14.
 */
public class ApplicationDescriptor extends AbstractDescriptor implements Writable {
    public String packageName;
    public String lastUpdated;
    public String version;
    public int build;
    public String developerContact;
    public String appstoreId;
    public String appdfLink;
    public String descriptionLink;
    public int currPageHash;
    public int prevPageHash;

    @Override
    public void write(ResponseWriter responseWriter) throws WriteException {
        responseWriter.write(this);
    }
}
