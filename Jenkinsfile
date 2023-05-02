pipeline {
    agent { 
        kubernetes{
            inheritFrom 'jenkins-slave'
        }
    }
    options {
        timeout(time: 45, unit: 'MINUTES')
    }
    environment{
        AWSID = credentials('AWSID')
        DOCKER_PSW = credentials('DOCKER_PASSWORD')
        DOCKER_CONFIG = "${WORKSPACE}/docker.config"
        ORACLE_HOST = 'dotoradb.fount'
        ORACLE_PORT = 1521
        ORACLE_SID = credentials('ORACLE_SID')
        ORACLE_USER = credentials('ORACLE_USER')
        ORACLE_PASS = credentials('ORACLE_PASS')
        NAMESPACE = 'apps'
        APP_NAME = 'kinome-tree'
        AWS_PAGER = ''
    }

    stages {
        stage('docker login') {
            steps {
                script {
                    withCredentials([aws(credentialsId: 'awscredentials', region: 'us-west-2')]) {
                    sh '''
                        aws ecr get-login-password \
                        --region us-west-2 \
                        | docker login --username AWS \
                        --password-stdin $AWSID.dkr.ecr.us-west-2.amazonaws.com
                       '''
                    }
                }
            }
        }

        stage('docker build shiny app') {
            steps{
               sh( label: 'Docker Build', script:
               '''
                #!/bin/bash
                set -x
                docker build \
                --no-cache --network=host \
                -t ${AWSID}.dkr.ecr.us-west-2.amazonaws.com/$APP_NAME:latest \
                --build-arg HOSTNAME=${ORACLE_HOST} \
                --build-arg PORT=${ORACLE_PORT} \
                --build-arg SID=${ORACLE_SID} \
                --build-arg USERNAME=${ORACLE_USER} \
                --build-arg PASSWORD=${ORACLE_PASS} \
                .
                ''', returnStdout: true
                )
                
            }
        }
        

        stage('docker push shiny app to ecr') {
          steps {
                sh(label: 'ECR docker push backend', script:
                '''
                #!/bin/bash
                set -x
                docker push $AWSID.dkr.ecr.us-west-2.amazonaws.com/$APP_NAME:latest
                ''', returnStdout: true
                )
            }
        }
        
        stage('deploy') {
                agent {
                    kubernetes {
                      yaml '''
                        apiVersion: v1
                        kind: Pod
                        spec:
                          containers:
                          - name: helm
                            image: alpine/helm:3.11.1
                            command:
                            - cat
                            tty: true
                        '''
                        }
            }
            steps{
                container('helm') {
                sh script: '''
                #!/bin/bash
                set -x
                curl -LO https://storage.googleapis.com/kubernetes-release/release/\$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl
                chmod +x ./kubectl
                if ./kubectl get pod -n $NAMESPACE -l app=$APP_NAME | grep -q $APP_NAME; then
                  echo "$APP_NAME pods already exists"
                  ./kubectl rollout restart deploy/$APP_NAME-deploy -n $NAMESPACE
                else
                  echo "pods $APP_NAME do not exist; deploy using helm"
                  git clone https://github.com/sktrinh12/helm-basic-app-chart.git
                  cd helm-basic-app-chart
                  helm install k8sapp-$APP_NAME . --namespace $NAMESPACE --set service.namespace=$NAMESPACE \
                  --set service.port=80 --set nameOverride=$APP_NAME \
                  --set fullnameOverride=$APP_NAME --set namespace=${NAMESPACE} \
                  --set image.repository=${AWSID}.dkr.ecr.us-west-2.amazonaws.com/$APP_NAME \
                  --set image.tag=latest --set containers.name=shiny \
                  --set containers.ports.containerPort=80 --set app=$APP_NAME \
                  --set terminationGracePeriodSeconds=10 \
                  --set ingress.enabled=false --set service.type=ClusterIP
                fi
                '''

            }
        }
    }
    
    stage ('purge ecr untagged images') {
            steps {
                withCredentials([aws(credentialsId: 'awscredentials', region: 'us-west-2')]) {
                    loop_ecr_purge()
                }
            }
        }
        
    
    }
}

def loop_ecr_purge() {
    for (int i = 0; i < list.size(); i++) {
        sh """aws ecr list-images \
        --repository-name $APP_NAME \
        --filter 'tagStatus=UNTAGGED' \
        --query 'imageIds[].imageDigest' \
        --output json \
        | jq -r '.[]' \
        | xargs -I{} aws ecr batch-delete-image \
        --repository-name $APP_NAME \
        --image-ids imageDigest={} 
        """
        sh 'sleep 1'
    }
}
