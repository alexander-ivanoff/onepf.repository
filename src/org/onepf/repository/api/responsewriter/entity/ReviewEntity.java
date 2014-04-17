package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * Created by akarimova on 16.04.14.
 */
@Entity
@Table(name = "reviews")
public class ReviewEntity extends BaseEntity {
    @Column(name = "package")
    private String packageName;

    @Column(name = "version")
    private String version;

    @Column(name = "versionCode")
    private int build;

    @Column(name = "deviceModel")
    private String deviceModel;

    @Column(name = "deviceName")
    private String deviceName;

    @Column(name = "country")
    private String country;

    @Column(name = "rating")
    private int rating;

    @Column(name = "userName")
    private String userName;

    @Column(name = "reviewUrl")
    private String userUrl;

    @Column(name = "title")
    private String title;

    @Column(name = "textBody")
    private String body;

    public String getPackageName() {
        return packageName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
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

    public String getDeviceModel() {
        return deviceModel;
    }

    public void setDeviceModel(String deviceModel) {
        this.deviceModel = deviceModel;
    }

    public String getDeviceName() {
        return deviceName;
    }

    public void setDeviceName(String deviceName) {
        this.deviceName = deviceName;
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public int getRating() {
        return rating;
    }

    public void setRating(int stars) {
        this.rating = stars;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserUrl() {
        return userUrl;
    }

    public void setUserUrl(String userUrl) {
        this.userUrl = userUrl;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
    }

    @Id
    private int id;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
}
