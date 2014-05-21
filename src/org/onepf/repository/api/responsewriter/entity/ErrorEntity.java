package org.onepf.repository.api.responsewriter.entity;

import org.onepf.repository.api.xmlapi.JaxbElementMaker;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;

/**
 * Created by ivanoff on 08.05.14.
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "errorType")
public class ErrorEntity extends BaseEntity implements JaxbElementMaker {

    public static final int ERROR_CODE_BAD_REQUEST = 400;
    public static final int ERROR_CODE_UNAUTHORIZED = 401;
    public static final int ERROR_CODE_INTERNAL_ERROR = 500;

    @XmlAttribute(name = "version", required = true)
    int version;

    @XmlAttribute(name = "code", required = false)
    int code;

    @XmlAttribute(name = "description", required = false)
    String description;


    @Override
    public JAXBElement<ErrorEntity> getAsJaxbElement(QName qname) {
        return new JAXBElement<ErrorEntity>(qname, ErrorEntity.class, null, this);
    }

    public int getVersion() {
        return version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
