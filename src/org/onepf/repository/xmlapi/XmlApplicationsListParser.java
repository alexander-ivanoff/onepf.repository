package org.onepf.repository.xmlapi;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationListHeaderDescriptor;

import java.util.Collection;

/**
 * Created by ivanoff on 02.04.14.
 */
public class XmlApplicationsListParser extends  XmlListParser<ApplicationDescriptor, ApplicationListHeaderDescriptor> {


    public XmlApplicationsListParser(Collection<ApplicationDescriptor> items) {
        super(items, new XmlApplicationItemParser(), new XmlApplicationHeaderParser());
    }
}
