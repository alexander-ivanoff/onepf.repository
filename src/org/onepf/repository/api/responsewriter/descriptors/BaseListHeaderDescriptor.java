package org.onepf.repository.api.responsewriter.descriptors;

/**
* @author Alexander Ivanoff on 02.04.14.
 */
public class BaseListHeaderDescriptor extends AbstractDescriptor {

    public  String version;
    public String offset;

    public BaseListHeaderDescriptor(String version, String offset) {
        this.version = version;
        this.offset = offset;
    }

}
