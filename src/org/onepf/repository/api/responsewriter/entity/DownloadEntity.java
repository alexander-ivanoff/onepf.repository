package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.*;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * Created by akarimova on 16.04.14.
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "downloadType")
@Entity
@Table(name = "downloads")
public class DownloadEntity extends BaseStatisticEntity{

    @XmlAttribute(name = "is-update")
    @Column(name = "isUpdate")
    private String isUpdate;

    public String getIsUpdate() {
        return isUpdate;
    }

    public void setIsUpdate(String isUpdate) {
        this.isUpdate = isUpdate;
    }

}
