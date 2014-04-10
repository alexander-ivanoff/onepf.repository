package org.onepf.repository.api.responsewriter.descriptors;

import org.onepf.repository.api.responsewriter.ResponseWriter;
import org.onepf.repository.api.responsewriter.Writable;
import org.onepf.repository.api.responsewriter.WriteException;

/**
 * This class contains application description
 *
 * @author Alexander Ivanoff
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
    public String appdfHash;

    @Override
    public void write(ResponseWriter responseWriter) throws WriteException {
        responseWriter.write(this);
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof ApplicationDescriptor) {
            return packageName.equals(((ApplicationDescriptor)o).packageName);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return packageName.hashCode();
    }
}
