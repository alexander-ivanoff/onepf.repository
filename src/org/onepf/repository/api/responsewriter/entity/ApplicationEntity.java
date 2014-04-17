package org.onepf.repository.api.responsewriter.entity;


import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * Created by akarimova on 16.04.14.
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "applicationType")
@Entity
@Table(name = "applications")
public class ApplicationEntity extends BaseEntity {
    @Id
    private int id;

    @XmlAttribute(name = "package", required = true)
    @Column(name = "package")
    private String packageName;

    @XmlAttribute(name = "hash", required = true)
    @Column(name = "hash")
    private String appdfHash;

    @Column(name = "version")
    private String version;

    @Column(name = "versionCode")
    private int build;

    @Column(name = "datetime")
    private String datetime;

    @Column(name = "appstoreId")
    private String appstoreId;

    @Column(name = "appdfLink")
    private String appdfLink;


    //todo wtf
    @Override
    public boolean equals(Object o) {
        if (o instanceof ApplicationEntity) {
            return getPackageName().equals(((ApplicationEntity) o).packageName);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return getPackageName().hashCode();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getPackageName() {
        return packageName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public String getDatetime() {
        return datetime;
    }

    public void setDatetime(String lastUpdated) {
        this.datetime = lastUpdated;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public int getBuild() {
        return build;
    }

    public void setBuild(int build) {
        this.build = build;
    }

    public String getAppstoreId() {
        return appstoreId;
    }

    public void setAppstoreId(String appstoreId) {
        this.appstoreId = appstoreId;
    }

    public String getAppdfLink() {
        return appdfLink;
    }

    public void setAppdfLink(String appdfLink) {
        this.appdfLink = appdfLink;
    }

    public String getAppdfHash() {
        return appdfHash;
    }

    public void setAppdfHash(String appdfHash) {
        this.appdfHash = appdfHash;
    }

}
