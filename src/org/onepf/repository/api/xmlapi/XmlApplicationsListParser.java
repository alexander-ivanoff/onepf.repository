package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.responsewriter.descriptors.ApplicationListHeaderDescriptor;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;

import java.util.Collection;

/**
 *
 * XmlListParser realization to parse list of applications
 *
 * @author Alexander Ivanoff on 02.04.14.
 */
public class XmlApplicationsListParser extends  XmlListParser<ApplicationEntity, ApplicationListHeaderDescriptor> {

    public XmlApplicationsListParser(Collection<ApplicationEntity> items) {
        super(items, new XmlApplicationItemParser(), new XmlApplicationHeaderParser());
    }
}
