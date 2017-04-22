const aws = require("aws-sdk");
const s3params = { Bucket: process.env.AWS_BUCKET };
const s3bucket = new aws.S3({ params: s3params });

module.exports.uploadLessonData = async (lesson, data) => {
    return new Promise((resolve, reject) => {
        let params = {
            Key: `lessons/${lesson.id}`,
            Body: data
        };
        s3bucket.upload(params, err => {
            if (err) reject(err);
            else resolve();
        });
    });
};

module.exports.getLessonData = async lesson => {
    return new Promise((resolve, reject) => {
        let params = {
            Key: `lessons/${lesson.id}`
        };
        s3bucket.getObject(params, function(err, data) {
            if (err) reject(err);
            else resolve(data.Body.toString());
        });
    });
};
