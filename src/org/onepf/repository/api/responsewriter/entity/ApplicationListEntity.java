//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.04.16 at 06:46:48 PM MSK 
//


package org.onepf.repository.api.responsewriter.entity;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.List;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "application-listType", propOrder = {
    "application"
})
public class ApplicationListEntity extends BaseListEntity{

    protected List<ApplicationEntity> application;

    /**
     * Gets the value of the application property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the application property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getApplication().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ApplicationEntity }
     * 
     * 
     */
    public List<ApplicationEntity> getApplication() {
        if (application == null) {
            application = new ArrayList<ApplicationEntity>();
        }
        return this.application;
    }

    @Override
    public JAXBElement<ApplicationListEntity> getAsJaxbElement(QName qname) {
        return new JAXBElement<ApplicationListEntity>(qname, ApplicationListEntity.class, null, this);
    }
}
