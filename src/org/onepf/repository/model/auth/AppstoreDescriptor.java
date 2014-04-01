package org.onepf.repository.model.auth;

import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.WriteException;
import org.onepf.repository.utils.responsewriter.descriptors.AbstractDescriptor;

/**
 * Created by ivanoff on 12.03.14.
 */
public class AppstoreDescriptor extends AbstractDescriptor{
    public String authToken;
    public String appstoreId;
    public String description;
    public String publickKey;
    public String openaepUrl;
}
