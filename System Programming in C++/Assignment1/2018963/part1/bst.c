#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include <unistd.h>
#include "bst.h"

// Your source code here
Node* insertNode(Node* root, int value) {
    
    if (root == NULL)
    {
        Node* new = (Node*)malloc(sizeof(struct Node));
        new->data = value;
        new->right = new->left = NULL;
        return new;
    }
    if (value > root->data)
    {
        root->right = insertNode(root->right, value);
    }
    else
    {
        root->left = insertNode(root->left, value);
    }
    return root;
}

Node* deleteNode(Node* root, int value) {

    {
        if (root == NULL)
        {
            return root;
        }

        if (value < root->data)
        {
            root->left = deleteNode(root->left, value);
        }
        else if (value > root->data)
        {
            root->right = deleteNode(root->right, value);
        }

        else
        {
            Node* tmp = root;
            if (root->left != NULL)
            {
                for (tmp = root->left; tmp->right != NULL; tmp = tmp->right);
                int t;
                t = root->data;
                root->data = tmp->data;
                tmp->data = t;
                root->left = deleteNode(root->left, tmp->data);
            }
            else if (root->right != NULL)
            {
                for (tmp = root->right; tmp->left != NULL; tmp = tmp->left);
                int t;
                t = root->data;
                root->data = tmp->data;
                tmp->data = t;

                root->right = deleteNode(root->left, tmp->data);
            }
            else
            {
                free(root);
               return NULL;
            }
        }
        return root;
    }
}


void printSubtree(Node* N) {
    if (N->left != NULL) {
        printSubtree(N->left);
    }

    printf("%d\n", N-> data);

    if (N->right != NULL) {
        printSubtree(N->right);
    }

}


int countNodes(Node* N) {
    if (N == NULL) {
        return 0;
    }
    else {
        return 1 + countNodes(N->left) + countNodes(N->right);
    }
}

Node* freeSubtree(Node* N) {
    if (N) {
        freeSubtree(N->left);
        freeSubtree(N->right);
	free(N);
	N = NULL;
    }
	return N;
}

int sumSubtree(Node* N) {
    int sum;
    sum = 0;
    if (N != NULL) {
        return sumSubtree(N->left) + N->data + sumSubtree(N->right);
    }
    return sum;
}


