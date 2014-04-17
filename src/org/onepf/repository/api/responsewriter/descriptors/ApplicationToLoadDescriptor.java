package org.onepf.repository.api.responsewriter.descriptors;

/**
 * Created by ivanoff on 14.04.14.
 */

import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;

/**
 * This class contains application description
 *
 * @author Alexander Ivanoff
 */
public class ApplicationToLoadDescriptor extends AbstractDescriptor {
    public String packageName;
    public String appdfHash;
    public String appstoreId;
    public int status;
    public int nFailed;

    @Override
    public boolean equals(Object o) {
        if (o instanceof ApplicationEntity) {
            return packageName.equals(((ApplicationEntity)o).getPackageName());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return packageName.hashCode();
    }
}
